# Fix GitHub Auth Error On A New Laptop

Error message:

```
remote: Invalid username or token.
Password authentication is not supported for Git operations.
```

This means Git is trying to use HTTPS + password. GitHub no longer accepts account passwords for Git operations.

## Recommended fix (SSH)

1. Check current remote:

```bash
git remote -v
```

2. Create an SSH key (if you do not already have one):

```bash
ssh-keygen -t ed25519 -C "your_email@biochem.mpg.de"
```

3. Start agent and add the key:

```bash
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519
```

4. Copy public key and add it to GitHub:

```bash
cat ~/.ssh/id_ed25519.pub
```

GitHub: `Settings -> SSH and GPG keys -> New SSH key`

5. Switch repo remote to SSH:

```bash
git remote set-url origin git@github.com:yeroslaviz/BCFNGS-project-management.git
```

6. Test and pull:

```bash
ssh -T git@github.com
git pull origin main
```

## Fallback fix (HTTPS + PAT)

If SSH is not possible, use a Personal Access Token (PAT), not your account password.

1. Keep/set HTTPS remote:

```bash
git remote set-url origin https://github.com/yeroslaviz/BCFNGS-project-management.git
```

2. Pull/push and enter:
- Username: your GitHub username
- Password: your PAT token

3. Optional (store credentials on macOS keychain):

```bash
git config --global credential.helper osxkeychain
```

## Quick sanity check

```bash
git remote -v
git fetch origin
git status
```

## If `git pull` is blocked by local changes

Example error:

```text
error: cannot pull with rebase: You have unstaged changes.
error: Please commit or stash them.
```

### 1) Inspect what changed (safe)

```bash
# local unstaged/staged changes
git status -sb
git diff -- sequencing-app/app.R

# check what exists on remote
git fetch origin
git log --oneline HEAD..origin/main

# compare your file against remote tip
git diff origin/main -- sequencing-app/app.R
```

### 2) Keep your local changes, then pull

```bash
git stash push -m "wip before pull"
git pull origin main
git stash pop
```

### 3) Commit your local changes, then pull

```bash
git add sequencing-app/app.R
git commit -m "Local changes before pull"
git pull origin main
```

### 4) Discard local changes, then pull

```bash
# discard one file
git restore sequencing-app/app.R

# OR discard all tracked-file changes in repo
git restore --worktree --staged .

git pull origin main
```

### 5) Optional: avoid rebase-style pull on this machine

```bash
git config --global pull.rebase false
```
