"""Render YAML spec coverage JSON into a GitHub Actions job summary."""

import json
import os
from pathlib import Path


def append_summary(lines: list[str]) -> None:
    summary_path = os.environ.get("GITHUB_STEP_SUMMARY")
    if not summary_path:
        print("\n".join(lines))
        return
    with open(summary_path, "a", encoding="utf-8") as summary_file:
        summary_file.write("\n".join(lines) + "\n")


def load_report(path: Path) -> dict | None:
    if not path.exists():
        return None
    return json.loads(path.read_text(encoding="utf-8"))


def bar(passed: int, total: int, width: int = 24) -> str:
    if total <= 0:
        return "—" * width
    filled = round(width * passed / total)
    return "█" * filled + "░" * (width - filled)


def id_list(ids: list[str], limit: int = 12) -> str:
    if not ids:
        return "_none_"
    shown = ids[:limit]
    text = " ".join(f"`{item}`" for item in shown)
    extra = len(ids) - len(shown)
    if extra > 0:
        text += f" +{extra} more"
    return text


def append_report(lines: list[str], title: str, report: dict | None) -> None:
    lines.extend([f"### {title}", ""])
    if report is None:
        lines.extend(["Coverage report was not generated.", ""])
        return

    passed = int(report.get("passed", 0))
    failed = int(report.get("failed", 0))
    unsupported = int(report.get("unsupported", 0))
    total = int(report.get("total", 0))
    coverage = float(report.get("coverage_percent", 0.0))

    lines.extend(
        [
            f"`{bar(passed, total)}` **{coverage:.2f}%** ({passed}/{total})",
            "",
            "| Passed | Failed | Unsupported | Discovered |",
            "|-------:|-------:|------------:|-----------:|",
            f"| {passed} | {failed} | {unsupported} | {total} |",
            "",
        ]
    )

    failures = report.get("failures") or []
    if failures:
        lines.extend(["#### Failures", "", "| Case | Detail |", "| --- | --- |"])
        for failure in failures:
            detail = str(failure.get("detail", "")).replace("|", "\\|").replace("\n", " ")
            lines.append(f"| `{failure.get('id', '')}` | {detail} |")
        lines.append("")

    groups = report.get("unsupported_groups") or []
    if groups:
        lines.extend(
            [
                "#### Gaps",
                "",
                "Cases with no JSON oracle are valid or event-only fixtures this harness does not score yet. Other rows are parser errors on documents that do have an `in.json`.",
                "",
                "| Reason | Count | Cases |",
                "| --- | ---: | --- |",
            ]
        )
        for group in groups:
            ids = group.get("ids") or []
            lines.append(
                f"| `{group.get('reason', '')}` | {group.get('count', len(ids))} | {id_list(ids)} |"
            )
        lines.append("")

        long_groups = [group for group in groups if len(group.get("ids") or []) > 12]
        if long_groups:
            for group in long_groups:
                ids = group.get("ids") or []
                lines.append(f"<details><summary>{group.get('reason', '')} ({len(ids)})</summary>")
                lines.append("")
                lines.append(" ".join(f"`{item}`" for item in ids))
                lines.append("")
                lines.append("</details>")
                lines.append("")


def main() -> int:
    matrix_os = os.environ.get("MATRIX_OS", "unknown-os")
    lines = [f"## YAML spec coverage — `{matrix_os}`", ""]
    append_report(lines, "Pure Zig (`yaml`)", load_report(Path("zig-out/spec-coverage/coverage.json")))
    append_report(lines, "libfyaml (`fy`)", load_report(Path("zig-out/spec-coverage/coverage-fy.json")))
    append_summary(lines)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
