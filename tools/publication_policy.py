"""Reject non-source files and obvious private material in new Git objects."""

import argparse
import re
import subprocess
import sys
from pathlib import PurePosixPath


# These images were already public. Their existing bytes are preserved, while
# new or changed binaries require a deliberate policy change.
LEGACY_IMAGES = {
    "Sample_Output/SpecCurve.png": "08c1e6dd3db34f5e976bb7c7ac035a30d9a74f6d",
    "Sample_Output/spec_curve_pca_liq_b.png": "f9c418f1f52c6c4ddd8e547c7cfb306260cad45e",
    "Sample_Output/spec_curve_treat.png": "8ffb49133ae31d1725f8288aee39bd0489ade3cc",
}
EXACT_PATHS = {
    ".gitignore", ".renvignore", "CITATION.cff", "LICENSE", "readme.md", "Makefile", "renv.lock",
    "Sample Specification Curves.code-workspace",
    "spec_curve_print_curve_sample.R", "spec_curve_run_models_sample.R",
    ".githooks/pre-commit", ".githooks/pre-push",
    ".github/workflows/check.yml", "paper/run.R",
    "paper/scripts/acquire_mitton_materials.py",
    "paper/config/demo.json", "paper/config/applications.json",
        "tools/publication_policy.py", "tools/test_publication_policy.py",
    "tools/compare_implementations.py",
    "paper/config/menu.schema.json", "paper/reproduce.sh",
    "docs/ai-guide.md", "docs/templates/agent-instructions.md",
    "python/pyproject.toml", "stata/speccurve_run.do",
}
# Directory patterns: (parent parts, allowed suffixes). Application folders hold
# only scripts, menus and documentation (and, for GMM applications, their moment
# functions and published-value check); data never live in the repository.
PATTERN_PATHS = (
    (("paper", "R"), (".R",)),
    (("paper", "tests"), (".R",)),
    (("paper", "figures"), (".R",)),
    (("docs", "prompts"), (".md",)),
    (("python", "speccurve"), (".py",)),
    (("python", "tests"), (".py",)),
)
APPLICATION_FILES = {"README.md", "build_input.R", "menu.json", "moments.R", "moments.py", "check_published.R"}
PRIVATE_NAMES = {
    "data", "raw", "processed", "notes", "research", "transcripts",
    "conversations", "logs", "output", "outputs", "results", "scratch",
    ".paper-agent", ".env", ".renviron", "agents.md", "claude.md",
    "project.yaml", "rules.yaml", "project_memory.md",
    # The manuscript is kept private; only code and code documentation are public.
    "manuscript", "submission", "highlights", "cover_letter", "supplement",
}
PRIVATE_CONTENT = (
    re.compile(r"(?mi)^\s*(?:user|assistant|system)\s*:\s+"),
    re.compile(r'"role"\s*:\s*"(?:user|assistant|system)"'),
    re.compile(r"-----BEGIN (?:RSA |OPENSSH |EC )?PRIVATE KEY-----"),
    re.compile(r"\b(?:gh[pousr]_[A-Za-z0-9]{30,}|github_pat_[A-Za-z0-9_]{30,})\b"),
    re.compile(r"/" r"Users/[^/\s]+/"),
)


def git(*args):
    return subprocess.check_output(["git", *args])


def allowed_path(path):
    parts = PurePosixPath(path).parts
    if any(part.lower() in PRIVATE_NAMES for part in parts):
        return False
    if PurePosixPath(path).stem.lower() in PRIVATE_NAMES:
        return False
    if path in EXACT_PATHS or path in LEGACY_IMAGES:
        return True
    for parent, suffixes in PATTERN_PATHS:
        if parts[:-1] == parent and parts[-1].endswith(suffixes):
            return True
    return len(parts) == 4 and parts[:2] == ("paper", "applications") and parts[-1] in APPLICATION_FILES


def inspect_blob(path, mode, oid, content):
    if mode not in ("100644", "100755"):
        return "symlinks and submodules are not publication sources"
    if not allowed_path(path):
        return "path is outside the source allowlist"
    if path in LEGACY_IMAGES:
        return None if oid == LEGACY_IMAGES[path] else "changed legacy binary"
    if len(content) > 1_000_000 or b"\x00" in content:
        return "binary or oversized source file"
    try:
        text = content.decode("utf-8")
    except UnicodeDecodeError:
        return "source must be UTF-8 text"
    if any(pattern.search(text) for pattern in PRIVATE_CONTENT):
        return "content resembles a transcript, credential, or private local path"
    return None


def entries(ref=None, staged=False):
    if ref is not None:
        for entry in git("ls-tree", "-r", "-z", ref).split(b"\x00"):
            if entry:
                meta, path = entry.split(b"\t", 1)
                mode, kind, oid = meta.decode().split()
                yield path.decode(), mode, oid
    else:
        changed = None
        if staged:
            changed = set(git("diff", "--cached", "--name-only", "--diff-filter=ACMR", "-z").split(b"\x00"))
        for entry in git("ls-files", "--stage", "-z").split(b"\x00"):
            if entry:
                meta, path = entry.split(b"\t", 1)
                mode, oid, stage = meta.decode().split()
                if stage != "0":
                    raise ValueError("Resolve index conflicts before publication")
                if changed is None or path in changed:
                    yield path.decode(), mode, oid


def audit(ref=None, staged=False):
    errors = []
    for path, mode, oid in entries(ref, staged):
        content = git("cat-file", "blob", oid) if mode in ("100644", "100755") else b""
        reason = inspect_blob(path, mode, oid, content)
        if reason:
            errors.append(f"{path}: {reason}")
    return errors


def outgoing_commits():
    commits = set()
    for line in sys.stdin:
        local_ref, local_sha, remote_ref, remote_sha = line.split()
        if set(local_sha) == {"0"}:
            continue
        # Inspect intermediate commits too: deleting a private file later would
        # still expose it in the pushed history.
        args = ["rev-list", local_sha, "--not", "--remotes=origin"]
        if set(remote_sha) != {"0"}:
            args.append(remote_sha)
        commits.update(git(*args).decode().split())
    return sorted(commits)


def main():
    parser = argparse.ArgumentParser()
    group = parser.add_mutually_exclusive_group()
    group.add_argument("--staged", action="store_true")
    group.add_argument("--pre-push", action="store_true")
    group.add_argument("--ref")
    args = parser.parse_args()
    errors = []
    if args.pre_push:
        for commit in outgoing_commits():
            errors.extend(f"{commit[:12]} {error}" for error in audit(ref=commit))
    else:
        errors = audit(ref=args.ref, staged=args.staged)
    if errors:
        print("Publication check failed:\n" + "\n".join(errors), file=sys.stderr)
        return 1
    print("Publication check passed: source allowlist and content scan.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
