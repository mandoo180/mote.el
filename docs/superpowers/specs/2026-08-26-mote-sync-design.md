# mote.el 설계 — 개인 노트 git 동기화

- 작성일: 2026-08-26
- 상태: 승인됨 (구현 계획 작성 대기)

## 1. 개요

`mote.el` 은 개인 노트 디렉터리 하나를 git 저장소로 관리하고, 단 하나의 명령
`mote-sync` 로 "로컬 커밋 → 원격 병합 → 푸시" 전체를 처리하는 Emacs 패키지다.

설계 원칙:

1. **사용자는 git 트러블을 다루지 않는다.** 충돌, 중단된 병합, 잠금 파일, 미설정
   identity 등은 모두 패키지가 스스로 복구한다. 프롬프트를 띄우지 않는다.
2. **되돌릴 수 있어야 한다.** 자동 해결이 잘못된 선택을 했더라도 git 이력에 양쪽이
   모두 남아 있어야 한다.
3. **Emacs 를 멈추지 않는다.** 모든 git 호출은 비동기다.

### 목표

- `mote-root` 가 가리키는 단일 git 저장소의 양방향 동기화
- 포맷이 고정된 자동 커밋 메시지
- 충돌 시 "파일별로 더 최신 커밋이 이긴다" 규칙의 무인 해결
- 반복 실행만으로 이전 실패에서 자동 복구

### 비목표

- 여러 저장소 / 여러 루트 동기화
- 자동 타이머, `after-save-hook` 연동 (수동 호출 전용)
- git submodule, git-lfs, 암호화, 비밀 관리
- 병합 결과의 대화형 검토 UI

## 2. 사용자 인터페이스

### 2.1 공개 심볼

공개 함수는 `mote-sync` **하나뿐**이다. 그 외 모든 함수·변수는 `mote--` 접두사를
가지는 내부 심볼이다.

| 변수 | 타입 | 기본값 | 역할 |
|---|---|---|---|
| `mote-root` | directory | `"~/mote-sync"` | 노트 루트이자 git 저장소 |
| `mote-remote` | string \| nil | `nil` | 원격 URL. nil 이면 로컬 커밋만 하고 정상 종료 |
| `mote-branch` | string | `"main"` | 동기화 대상 브랜치 |
| `mote-gitignore` | list of string | 아래 참조 | `git init` 시 한 번만 시드되는 `.gitignore` 라인 |
| `mote-push-retry-limit` | integer | `3` | push 거절 시 fetch→merge→push 재시도 상한 |
| `mote-log-buffer` | string | `"*mote-log*"` | 전체 git 명령·출력 보관 버퍼 이름 |

`mote-gitignore` 기본값: `(".DS_Store" "*~" "#*#" ".#*" "*.synctex.gz")`

원격 이름은 `"origin"` 으로 고정한다 (defcustom 아님).

모든 defcustom 은 `mote` 그룹에 속하며 `:type` 을 명시한다.

### 2.2 `mote-sync` 계약

- 인자 없는 `interactive` 명령.
- 호출 즉시 반환하고 백그라운드로 진행한다.
- 이미 세션이 진행 중이면 `mote: sync already in progress` 를 `message` 로 알리고
  아무것도 하지 않는다.
- 진행 중에는 각 스텝 시작 시 에코 영역에 `mote: <step>` 을 표시한다.
- 완료 시 한 줄 요약을 `message` 로 남긴다. 실패 시 `warn` 대신 `message` 로
  경고 한 줄 + `*mote-log*` 안내만 하고, 사용자 조치를 요구하지 않는다.

요약 메시지 예시:

```
mote: up to date
mote: 3 changes committed, pushed to origin/main
mote: merged 2 conflicts (latest wins), pushed to origin/main
mote: committed locally; remote unreachable (see *mote-log*)
```

### 2.3 커밋 메시지 포맷

**일반 동기화 커밋**

```
mote: sync macbook 2026-08-26 13:45 (+3 ~1 -0)

A	inbox/2026-08-26-idea.org
M	denote/20260826T101500--meeting.org
D	scratch.md
```

- subject: `mote: sync <HOST> <YYYY-MM-DD HH:MM> (+<추가> ~<수정> -<삭제>)`
- `<HOST>`: `(car (split-string (system-name) "\\."))`
- 시각: 로컬 시간대
- body: `git diff --cached --name-status -z` 의 결과. 20줄 상한, 초과 시 마지막에
  `… and N more` 한 줄 추가
- 통계 집계: `A` → 추가, `M`/`R`/`C`/`T` → 수정, `D` → 삭제

**부트스트랩 초기 커밋**

```
mote: init macbook 2026-08-26 13:45
```

**충돌을 해결한 병합 커밋**

```
mote: merge origin/main (latest-wins: 2 files)

resolved by newer commit time:
  local   denote/foo.org   2026-08-26 13:40
  remote  inbox/bar.org    2026-08-26 13:44
```

**충돌 없이 자동 병합된 경우**

```
mote: merge origin/main
```

## 3. 아키텍처

### 3.1 세션

동기화 1회 = 세션 1개. 전역 변수 `mote--session` 이 현재 세션을 담으며, non-nil
이면 새 `mote-sync` 호출은 거부된다. 재진입 방어는 이 변수 하나로 완결된다.

```elisp
(cl-defstruct mote--session
  root        ; expand-file-name 된 절대 경로
  queue       ; 남은 스텝 리스트. 각 원소는 (NAME . FUNCTION)
  proc        ; 현재 실행 중인 프로세스 (없으면 nil)
  timer       ; 현재 프로세스의 워치독 타이머
  log         ; 누적 로그 문자열 리스트 (역순)
  retries     ; push 재시도 횟수
  stats       ; (ADDED MODIFIED DELETED) 정수 3개
  conflicts   ; 해결 기록: (PATH SIDE LOCAL-TIME REMOTE-TIME) 리스트
  remote-p    ; 원격 동기화 수행 여부
  status      ; 최종 결과 심볼: 'ok 'local-only 'remote-failed 'error
  callback)   ; 완료 시 호출할 함수 (테스트/내부용, 인자는 세션)
```

### 3.2 프로세스 러너

```elisp
(mote--git SESSION ARGS HANDLER)
```

- `git -C <root> <ARGS...>` 를 `make-process` 로 실행한다.
- stdout/stderr 를 하나의 버퍼로 합쳐 캡처한다.
- 프로세스 환경에 다음을 강제한다:
  - `GIT_TERMINAL_PROMPT=0` — 자격증명 프롬프트 대신 즉시 실패
  - `GIT_ASKPASS=` / `SSH_ASKPASS=` — GUI 프롬프트 차단
  - `LC_ALL=C` — 출력 파싱이 로케일에 흔들리지 않도록 고정
- 워치독: `mote--process-timeout` (내부 상수, 120초) 후에도 살아 있으면
  `kill-process` 하고 해당 스텝을 실패로 처리한다.
- sentinel 이 `(funcall HANDLER SESSION EXIT-CODE OUTPUT)` 를 호출한다.
- 실행한 명령과 출력은 세션 로그와 `mote-log-buffer` 에 누적한다.

### 3.3 스텝 큐

- 스텝은 `(NAME . FUNCTION)` 쌍이고, `FUNCTION` 은 세션 하나를 인자로 받는다.
- `mote--next` 가 큐 앞에서 하나를 꺼내 실행한다. 큐가 비면 `mote--finish`.
- 핸들러가 아무 조치도 하지 않으면 자연히 큐의 다음 스텝으로 진행한다.
- `(mote--push-steps SESSION STEPS)` 로 큐 **앞**에 스텝들을 밀어 넣으면 분기와
  루프가 표현된다. 충돌 파일 N개 처리, push 재시도 등이 모두 이 한 가지
  메커니즘으로 해결된다.
- 스텝 함수는 git 호출 후 결과를 해석해 큐를 조작하는 것 외의 부수효과를 가지지
  않는다. 덕분에 스텝 단위 ERT 검증이 가능하다.

### 3.4 손상 방지 하드 룰

mote 는 다음 명령을 **어떤 경로에서도 실행하지 않는다**:

- `git reset --hard`
- `git clean`
- `git push --force` / `--force-with-lease`
- `git checkout` 의 파괴적 형태 중 충돌 해결 경로(§5) 밖의 사용

모든 복구는 이력을 보존하는 연산(`--abort`, 새 커밋, `git rm` 후 커밋)만 사용한다.
따라서 자동 해결이 잘못되었을 때 항상 `git revert` 또는 `git checkout <sha> -- <path>`
로 되돌릴 수 있다.

## 4. 파이프라인

### 4.1 정상 경로

```
preflight → detect → heal → stage → status → commit
          → remote-setup → fetch → merge-check → merge → [resolve …] → push → finish
```

### 4.2 스텝 명세

| 스텝 | 동작 | 분기 |
|---|---|---|
| `preflight` | elisp 전용. `mote-root` 없으면 `make-directory ... t`. `git` 실행파일 없으면 즉시 `error` 종료 | — |
| `detect` | `rev-parse --git-dir` | exit≠0 → 부트스트랩 스텝(§6.1)을 큐 앞에 push |
| `heal` | §6 자가 복구 점검 | 필요한 복구 스텝을 큐 앞에 push |
| `stage` | `add -A` | — |
| `status` | `diff --cached --name-status -z` → `stats` 계산 | 결과가 비면 `commit` 스킵 |
| `commit` | `commit --no-verify -m <subject> -m <body>` | exit 128 + identity 오류 → §6 항목 적용 후 재시도 |
| `remote-setup` | `remote get-url origin` | §6.2 참조. 원격이 전혀 없으면 `status` 를 `local-only` 로 두고 `finish` |
| `fetch` | `fetch --prune origin` | exit≠0 → `remote-failed` 로 `finish` |
| `merge-check` | `rev-parse --verify --quiet origin/<branch>` | exit≠0 → `merge`/`resolve` 건너뛰고 `push` |
| `merge` | `merge --no-edit -m "mote: merge origin/<branch>" origin/<branch>` | exit 0 → `push` / unrelated histories → 재시도 / 그 외 → `resolve` 스텝 push |
| `resolve` | §5 충돌 해결 | 파일당 4스텝 push, 종료 후 병합 커밋 |
| `push` | `push -u origin <branch>` | 거절 → §4.3 재시도 / 그 외 실패 → `remote-failed` 로 `finish` |
| `finish` | `mote--session` 을 nil 로, 요약 `message`, `callback` 호출 | — |

`commit --no-verify` 를 쓰는 이유: 저장소에 훅이 설치되어 있어도 무인 동작이
막히지 않게 하기 위함이다.

`--name-status -z` 파싱 시 주의: `R`/`C` 레코드는 상태 필드 뒤에 경로가 **두 개**
(원본, 대상) 이어진다. 파서는 상태 문자로 필드 개수를 결정해야 한다.

### 4.3 push 재시도

`push` 의 stderr 가 non-fast-forward 계열(`rejected`, `fetch first`,
`non-fast-forward`)이면:

- `retries` < `mote-push-retry-limit` → `retries` 를 1 증가시키고
  `[fetch, merge-check, merge, push]` 를 큐 앞에 push
- 상한 초과 → `remote-failed` 로 `finish`

인증 실패, 호스트 해석 실패 등 그 외 오류는 재시도하지 않고 즉시
`remote-failed` 로 `finish` 한다. 어느 경우든 **로컬 커밋은 그대로 보존**되며,
다음 `mote-sync` 호출이 이어서 복구한다.

## 5. 충돌 해결 — 파일별 latest wins

### 5.1 충돌 목록 수집

`merge` 가 실패하면 `status --porcelain=v2 -z` 를 실행하고 `u` 레코드를 파싱한다.
각 레코드에서 XY 코드와 경로를 얻는다. (`-z` 이므로 경로에 공백·개행이 있어도
안전하다.)

### 5.2 판정

파일 `P` 마다 두 번의 git 호출로 시각을 얻는다:

```
t_local  = git log -1 --format=%ct HEAD       -- P
t_remote = git log -1 --format=%ct MERGE_HEAD -- P
```

출력이 비어 있으면 `0` 으로 간주한다. `t_local >= t_remote` 이면 로컬 승,
아니면 원격 승 (동률은 로컬 우선).

### 5.3 적용

| XY | 의미 | 로컬 승 | 원격 승 |
|---|---|---|---|
| `UU` | 양쪽 수정 | `checkout --ours -- P` | `checkout --theirs -- P` |
| `AA` | 양쪽 추가 | `checkout --ours -- P` | `checkout --theirs -- P` |
| `DU` | 로컬 삭제 · 원격 수정 | `rm -- P` | `checkout MERGE_HEAD -- P` |
| `UD` | 로컬 수정 · 원격 삭제 | `checkout HEAD -- P` | `rm -- P` |
| `DD` | 양쪽 삭제 | `rm -- P` | `rm -- P` |

적용 후 `add -- P` (삭제 경로는 `rm` 이 이미 인덱스를 갱신하므로 생략).
각 파일 처리는 큐에 4개 스텝(`log`×2, 적용, `add`)으로 push 된다.

삭제도 "변경"으로 취급한다는 점에 유의한다. `git log -1 -- P` 는 해당 경로를
삭제한 커밋도 반환하므로, 삭제가 더 최신이면 삭제가 이긴다.

### 5.4 병합 커밋

모든 충돌 파일 처리가 끝나면 §2.3 의 병합 커밋 메시지로
`commit --no-verify -m <subject> -m <body>` 를 실행한다. `conflicts` 기록이
그대로 커밋 본문이 되므로, 어느 쪽이 왜 이겼는지가 이력에 남는다.

## 6. 자가 복구 매트릭스

아래 항목은 각각 담당 스텝에서 점검된다. 대부분은 `heal` 이 매 실행마다 선제적으로
확인하고, 나머지는 해당 git 명령이 실패했을 때 그 자리에서 복구된다. 어떤 항목도
사용자에게 묻지 않는다.

| # | 상황 | 담당 스텝 | 감지 | 복구 |
|---|---|---|---|---|
| 1 | root 디렉터리 없음 | `preflight` | `file-directory-p` | `make-directory ... t` |
| 2 | git 저장소 아님 | `detect` | `rev-parse --git-dir` ≠ 0 | §6.1 부트스트랩 |
| 3 | 커밋 0개 | `heal` | `rev-parse --verify HEAD` 실패 | `add -A` 후 `mote: init …` 커밋 |
| 4 | 이전 병합 중단 | `heal` | `.git/MERGE_HEAD` 존재 | `merge --abort` |
| 5 | rebase 중단 | `heal` | `.git/rebase-merge` 또는 `.git/rebase-apply` | `rebase --abort` |
| 6 | cherry-pick 중단 | `heal` | `.git/CHERRY_PICK_HEAD` | `cherry-pick --abort` |
| 7 | `index.lock` 잔존 | `heal` | 파일 존재 & mtime 120초 초과 | 파일 삭제 |
| 8 | detached HEAD | `heal` | `symbolic-ref -q HEAD` 실패 | `checkout <branch>`, 없으면 `checkout -b <branch>` |
| 9 | 브랜치 불일치 | `heal` | `rev-parse --abbrev-ref HEAD` ≠ `mote-branch` | `checkout <branch>`, 없으면 `checkout -b <branch>` |
| 10 | identity 미설정 | `commit` | exit 128 + stderr 에 `Please tell me who you are` | `-c user.name=mote -c user.email=mote@<HOST>` 로 재시도 |
| 11 | origin URL 불일치 | `remote-setup` | `remote get-url origin` ≠ `mote-remote` (후자 non-nil) | `remote set-url origin <mote-remote>` |
| 12 | origin 없음 | `remote-setup` | `remote get-url origin` 실패 | `mote-remote` non-nil → `remote add`; nil → `local-only` 로 종료 |
| 13 | 원격 브랜치 없음 | `merge-check` | `rev-parse --verify --quiet origin/<branch>` 실패 | `merge` 건너뛰고 `push -u` |
| 14 | unrelated histories | `merge` | stderr 에 `refusing to merge unrelated histories` | `--allow-unrelated-histories` 로 1회 재시도 |
| 15 | 네트워크·인증 실패 | `fetch` / `push` | exit ≠ 0 | 로컬 커밋 보존, `remote-failed` 로 종료 |

항목 7 의 mtime 조건은 다른 git 프로세스가 정상 작업 중인 잠금을 지우지 않기
위한 안전장치다.

### 6.1 부트스트랩

1. `init -b <branch>` — exit≠0 이면 (git < 2.28) `init` 후
   `symbolic-ref HEAD refs/heads/<branch>`
2. `.gitignore` 가 없으면 `mote-gitignore` 내용으로 생성 (이미 있으면 손대지 않음)
3. `add -A`
4. `commit --no-verify -m "mote: init <HOST> <date>"`

이후 정상 경로의 `remote-setup` 부터 이어진다.

### 6.2 원격 설정

| `mote-remote` | origin 존재 | 동작 |
|---|---|---|
| nil | 없음 | `local-only` 로 `finish` |
| nil | 있음 | 기존 origin 을 그대로 사용 |
| non-nil | 없음 | `remote add origin <mote-remote>` |
| non-nil | 있음, URL 동일 | 그대로 |
| non-nil | 있음, URL 다름 | `remote set-url origin <mote-remote>` |

## 7. 오류 처리와 보고

- 모든 git 명령과 출력은 `mote-log-buffer` 에 타임스탬프와 함께 누적된다.
  버퍼는 read-only 이며 사용자가 언제든 열어볼 수 있다.
- 세션이 `error` 로 끝나는 경우는 두 가지뿐이다: `git` 실행파일 부재,
  `mote-root` 생성 실패. 그 외 모든 실패는 `remote-failed` 또는 `local-only` 로
  **정상 종료**하며 다음 호출에서 복구된다.
- Emacs 가 세션 도중 종료되면 프로세스도 함께 죽는다. 남은 `MERGE_HEAD` 나
  `index.lock` 은 다음 실행의 `heal` 이 정리한다.

## 8. 테스트 계획

`test/mote-test.el`, ERT.

### 8.1 픽스처

임시 디렉터리에 bare 저장소(원격 역할) 1개와 클론 2개(머신 A/B 역할)를 만든다.
커밋 시각은 `GIT_COMMITTER_DATE` / `GIT_AUTHOR_DATE` 를 명시해 고정하므로
latest-wins 판정이 결정적이다.

비동기 완료 대기는 내부 진입점 `(mote--sync-1 ROOT CALLBACK)` 을 사용하고,
테스트 쪽에서 `(while (and (not done) (< elapsed limit)) (accept-process-output nil 0.05))`
로 폴링한다.

### 8.2 시나리오

1. 빈 디렉터리 → 부트스트랩 → 초기 커밋과 `.gitignore` 생성 확인
2. 변경 없음 → 빈 커밋이 생기지 않음
3. 로컬 변경 → 커밋 subject 포맷과 `(+A ~M -D)` 통계 일치
4. 원격이 앞서 있음 → fast-forward 병합 후 로컬에 반영
5. `UU` 충돌, 로컬이 더 최신 → 로컬 내용이 남음
6. `UU` 충돌, 원격이 더 최신 → 원격 내용이 남음
7. `DU` 충돌 (로컬 삭제 · 원격 수정) 양방향 판정
8. `UD` 충돌 (로컬 수정 · 원격 삭제) 양방향 판정
9. `.git/MERGE_HEAD` 를 남긴 상태에서 시작 → 자동 abort 후 정상 완료
10. 오래된 `index.lock` 잔존 → 제거 후 정상 완료
11. unrelated histories → `--allow-unrelated-histories` 재시도로 병합 성공
12. push 거절(그 사이 원격 갱신) → fetch 재시도 후 성공
13. 원격 unreachable → 로컬 커밋 보존, `remote-failed` 로 종료

### 8.3 검증 커맨드

```sh
emacs --batch -f batch-byte-compile mote.el
emacs --batch -l ert -l mote.el -l test/mote-test.el -f ert-run-tests-batch-and-exit
emacs --batch -l checkdoc -f checkdoc-file mote.el
```

바이트 컴파일 경고는 0 이어야 한다.

## 9. 파일 구성

```
mote.el                 단일 파일 구현 (~600줄)
test/mote-test.el       ERT 스위트
README.org              사용법
LICENSE                 GPLv3
.gitignore
docs/superpowers/specs/2026-08-26-mote-sync-design.md
```

`mote.el` 헤더: `lexical-binding: t`, `Package-Requires: ((emacs "28.1"))`,
GPLv3 고지 (형제 패키지 `org-html-preview` 와 동일한 형식).

`;;;` 섹션 구분:

1. Customization
2. Session state
3. Process runner
4. Steps — bootstrap & heal
5. Steps — local commit
6. Steps — remote sync
7. Conflict resolution
8. Reporting
9. Entry point

## 10. 배포 및 적용

1. `~/Projects/elisp/mote.el` 에서 `git init` 후 초기 커밋
2. `gh repo create mandoo180/mote.el --public --source=. --remote=origin --push`
3. `~/Projects/emacs.light.d/init.el` 에 추가:

```elisp
(use-package mote
  :vc (:url "https://github.com/mandoo180/mote.el" :rev :newest)
  :commands (mote-sync)
  :custom (mote-root (expand-file-name "~/mote-sync"))
  :bind ("C-c n S" . mote-sync))
```

`use-package-always-ensure t` 가 켜져 있으므로 `:vc` 와의 상호작용을 실제로
확인한다. 설치가 실패하면 `:ensure nil` 을 추가하거나
`:load-path "~/Projects/elisp/mote.el"` 로 폴백한다.

키 `C-c n s` 는 이미 `denote-sort-dired` 가 사용 중이므로 대문자 `S` 를 쓴다.

4. 배치 스모크 테스트 후, 실제 `M-x mote-sync` 로 `~/mote-sync` 부트스트랩을
   실증한다.
5. 노트 저장소의 원격(`mote-remote`)은 nil 로 시작한다. 사용자가 private 저장소를
   만든 뒤 지정한다.

## 11. 가정

- `mote-root` 는 `~/mote-sync` 이며 현재 비어 있고 git 저장소가 아니다.
  (`~/Documents` 하위는 Google Drive 동기화 대상이라 제외했다.)
- 기본 브랜치는 `main`.
- git 2.11 이상 (`status --porcelain=v2` 요구). 2.28 미만에서는 §6.1 의
  `symbolic-ref` 폴백이 동작한다.
- 인증은 SSH 키 또는 이미 설정된 credential helper 로 처리된다. mote 는 자격증명을
  다루지 않는다.
