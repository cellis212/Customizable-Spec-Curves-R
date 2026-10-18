import unittest
import os
from pathlib import Path
import subprocess
import sys
import tempfile

from publication_policy import LEGACY_IMAGES, allowed_path, inspect_blob


class PublicationPolicyTests(unittest.TestCase):
    def test_source_paths(self):
        for path in ["paper/run.R", "paper/R/curve.R", "paper/tests/test_curve.R", "renv.lock",
                     "paper/applications/bll2010/menu.json",
                     "paper/applications/cz_am/build_input.R", "docs/ai-guide.md", "docs/prompts/01-design-menu.md",
                     "python/speccurve/engine.py", "python/tests/test_engine.py", "stata/speccurve_run.do",
                     "paper/figures/make_figures.R", "tools/compare_implementations.py"]:
            self.assertTrue(allowed_path(path), path)

    def test_data_and_internal_records_fail_closed(self):
        for path in ["data/input.csv", "paper/R/data.csv", "paper/R/notes/secret.R",
                     "paper/notes.R", "paper/config/notes.json", "report/paper.qmd",
                     ".paper-agent/STATE.json", "AGENTS.md", "new-output.png",
                     "paper/manuscript/output/paper.pdf", "paper/manuscript/sections/notes.qmd",
                     "paper/applications/bll2010/bll_input.csv", "paper/applications/cz_am/results.csv",
                     "paper/figures/Figure_1.pdf", "python/speccurve/results.py", "docs/prompts/notes.md",
                     "paper/manuscript/generated/table_menu_bll.tex", "python/.venv/bin/python",
                     "paper/manuscript/paper.qmd", "paper/manuscript/references.bib",
                     "paper/manuscript/sections/05_applications.qmd", "manuscript/paper.qmd",
                     "paper/scripts/word_count.py", "docs/supplement.md", "paper/highlights.md"]:
            self.assertFalse(allowed_path(path), path)

    def test_forced_staging_cannot_allow_data(self):
        self.assertIsNotNone(inspect_blob("data/input.csv", "100644", "x", b"value\n1\n"))

    def test_transcript_in_source_is_rejected(self):
        content = ("User" + ": private discussion\n").encode()
        self.assertIsNotNone(inspect_blob("paper/R/model.R", "100644", "x", content))

    def test_symlinks_and_binary_are_rejected(self):
        self.assertIsNotNone(inspect_blob("paper/R/model.R", "120000", "x", b"target"))
        self.assertIsNotNone(inspect_blob("paper/R/model.R", "100644", "x", b"\x00"))

    def test_legacy_images_are_immutable(self):
        path, oid = next(iter(LEGACY_IMAGES.items()))
        self.assertIsNone(inspect_blob(path, "100644", oid, b"existing public image"))
        self.assertIsNotNone(inspect_blob(path, "100644", "new", b"replacement image"))


class GitIntegrationTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.repo = Path(self.temporary.name)
        self.env = dict(os.environ, GIT_CONFIG_GLOBAL=os.devnull, GIT_CONFIG_NOSYSTEM="1")
        self.command("git", "init", "--initial-branch=main")
        self.command("git", "config", "user.name", "Publication Test")
        self.command("git", "config", "user.email", "test@example.invalid")
        (self.repo / "paper/R").mkdir(parents=True)
        (self.repo / "paper/R/model.R").write_text("x <- 1\n")
        self.command("git", "add", ".")
        self.command("git", "commit", "-m", "Initial source")
        self.command("git", "update-ref", "refs/remotes/origin/main", "HEAD")
        self.policy = str(Path(__file__).with_name("publication_policy.py").resolve())

    def tearDown(self):
        self.temporary.cleanup()

    def command(self, *args):
        return subprocess.check_output(args, cwd=self.repo, env=self.env, stderr=subprocess.STDOUT)

    def test_forced_staged_data_is_rejected(self):
        (self.repo / "data.csv").write_text("value\n1\n")
        self.command("git", "add", "-f", "data.csv")
        result = subprocess.run([sys.executable, self.policy, "--staged"], cwd=self.repo,
                                env=self.env, capture_output=True, text=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("data.csv", result.stderr)

    def test_deleted_private_file_still_blocks_outgoing_history(self):
        (self.repo / "data.csv").write_text("value\n1\n")
        self.command("git", "add", "data.csv")
        self.command("git", "commit", "-m", "Add excluded test file")
        self.command("git", "rm", "data.csv")
        self.command("git", "commit", "-m", "Remove excluded test file")
        sha = self.command("git", "rev-parse", "HEAD").decode().strip()
        line = f"refs/heads/main {sha} refs/heads/main {'0' * 40}\n"
        result = subprocess.run([sys.executable, self.policy, "--pre-push"], cwd=self.repo,
                                env=self.env, input=line, capture_output=True, text=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("data.csv", result.stderr)


if __name__ == "__main__":
    unittest.main()
