import json
import os
from pathlib import Path


def append_summary(lines: list[str]) -> None:
    summary_path = os.environ.get("GITHUB_STEP_SUMMARY")
    if not summary_path:
        return
    with open(summary_path, "a", encoding="utf-8") as summary_file:
        summary_file.write("\n".join(lines) + "\n")


def bullet_list(ids: list[str]) -> str:
    if not ids:
        return "_none_"
    return " ".join(f"`{fid}`" for fid in ids)


def append_report_section(lines: list[str], title: str, report_path: Path) -> None:
    lines.extend([title, ""])

    if not report_path.exists():
        lines.append("Coverage report was not generated.")
        lines.append("")
        return

    report = json.loads(report_path.read_text(encoding="utf-8"))
    coverage = float(report.get("coverage_percent", 0.0))
    passed = report.get("passed", 0)
    total = report.get("total", 0)
    unsupported = report.get("unsupported", 0)
    failed = report.get("failed", 0)
    passed_ids = report.get("passed_ids", [])
    failed_ids = report.get("failed_ids", [])
    unsupported_ids = report.get("unsupported_ids", [])

    lines.extend(
        [
            f"> **{passed}/{total} ({coverage:.2f}%)**",
            "",
            "| Passed | Failed | Unsupported |",
            "|-------:|-------:|------------:|",
            f"| {passed} | {failed} | {unsupported} |",
            f"| {bullet_list(passed_ids)} | {bullet_list(failed_ids)} | {bullet_list(unsupported_ids)} |",
            "",
        ]
    )


def main() -> int:
    matrix_os = os.environ.get("MATRIX_OS", "unknown-os")
    lines: list[str] = [f"### YAML Spec Coverage — `{matrix_os}`", ""]

    append_report_section(lines, "#### yaml", Path("zig-out/spec-coverage/coverage.json"))
    append_report_section(lines, "#### fy", Path("zig-out/spec-coverage/coverage-fy.json"))

    append_summary(lines)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
