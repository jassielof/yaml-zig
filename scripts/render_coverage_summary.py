import json
import os
from pathlib import Path


def append_summary(lines: list[str]) -> None:
    summary_path = os.environ.get("GITHUB_STEP_SUMMARY")
    if not summary_path:
        return
    with open(summary_path, "a", encoding="utf-8") as summary_file:
        summary_file.write("\n".join(lines) + "\n")


def main() -> int:
    matrix_os = os.environ.get("MATRIX_OS", "unknown-os")
    report_path = Path("zig-out/spec-coverage/coverage.json")

    lines: list[str] = [f"## YAML Spec Coverage ({matrix_os})", ""]

    if not report_path.exists():
        lines.append("Coverage report was not generated.")
        append_summary(lines)
        return 0

    report = json.loads(report_path.read_text(encoding="utf-8"))
    coverage = float(report.get("coverage_percent", 0.0))
    passed = report.get("passed", 0)
    total = report.get("total", 0)
    unsupported = report.get("unsupported", 0)
    failed = report.get("failed", 0)
    failed_ids = report.get("failed_ids", [])
    unsupported_ids = report.get("unsupported_ids", [])

    lines.extend(
        [
            f"- coverage: **{coverage:.2f}%**",
            f"- passed: {passed} / {total}",
            f"- unsupported: {unsupported}",
            f"- failed: {failed}",
            "",
            "### Failed fixture IDs",
        ]
    )

    if not failed_ids:
        lines.append("- none")
    else:
        for fixture_id in failed_ids[:80]:
            lines.append(f"- {fixture_id}")
        if len(failed_ids) > 80:
            lines.append(f"- ... and {len(failed_ids) - 80} more")

    lines.extend(["", "### Unsupported fixture IDs"])
    if not unsupported_ids:
        lines.append("- none")
    else:
        for fixture_id in unsupported_ids[:80]:
            lines.append(f"- {fixture_id}")
        if len(unsupported_ids) > 80:
            lines.append(f"- ... and {len(unsupported_ids) - 80} more")

    append_summary(lines)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
