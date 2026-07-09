suite_app_css <- function() {
  "
  .skin-blue .main-header .navbar { background-color: #C3B1E1 !important; }
  .skin-blue .main-header .logo {
    background-color: #C3B1E1 !important;
    color: #fff !important;
    border-bottom: 1px solid #a892d0 !important;
    white-space: normal !important;
    line-height: 19px !important;
    font-size: 15px !important;
    padding-top: 5px !important;
  }
  .skin-blue .main-header .logo:hover { background-color: #a892d0 !important; }
  .skin-blue .main-header .navbar .sidebar-toggle { color: #fff !important; }
  .skin-blue .main-header .navbar .sidebar-toggle:hover { background-color: #a892d0 !important; }
  .skin-blue .main-header { border-bottom: 1px solid #a892d0 !important; }

  .skin-blue .main-sidebar { background-color: #ffffff !important; }
  .skin-blue .sidebar-menu > li > a { color: #333333 !important; }
  .skin-blue .sidebar-menu > li:hover > a {
    background-color: #E6E6FA !important;
    color: #000000 !important;
    border-left-color: #C3B1E1 !important;
  }
  .skin-blue .sidebar-menu > li.active > a {
    border-left-color: #C3B1E1 !important;
    background-color: #E6E6FA !important;
    color: #000000 !important;
  }

  .content-wrapper, .right-side { background-color: #ffffff !important; }
  .box {
    border: none !important;
    border-top: none !important;
    box-shadow: none !important;
    background: transparent !important;
  }
  .box > .box-header {
    background: transparent !important;
    color: #333 !important;
    padding: 6px 10px 4px !important;
    border-bottom: 1px solid #eee !important;
  }
  .box > .box-header .box-title {
    font-weight: 700 !important;
    font-size: 15px !important;
    color: #333 !important;
  }
  .box.box-solid.box-primary {
    background: #f6f6f8 !important;
    border-radius: 10px !important;
    padding-bottom: 6px !important;
  }
  .box.box-solid.box-primary > .box-header { border-bottom: 1px solid #e6e6ea !important; }
  .box.box-solid.box-primary > .box-body { background: #f6f6f8 !important; }
  .box.box-solid.box-warning { background: #ffffff !important; }
  .box.box-solid.box-warning > .box-body { background: #ffffff !important; }
  .box.box-solid.box-info,
  .box.box-solid.box-info > .box-body,
  .tab-content,
  .tab-pane { background: #ffffff !important; }

  .box { margin-bottom: 10px !important; }
  .col-sm-4 { padding-right: 8px !important; }
  .col-sm-8 { padding-left: 8px !important; }

  .nav-tabs > li > a { color: #C3B1E1 !important; }
  .nav-tabs > li.active > a,
  .nav-tabs > li.active > a:hover,
  .nav-tabs > li.active > a:focus { color: #000 !important; }
  .nav-tabs-custom > .nav-tabs > li.active { border-top-color: #C3B1E1 !important; }

  .suite-help {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 16px;
    height: 16px;
    border-radius: 50%;
    border: 1px solid #999;
    color: #666;
    background: #fff;
    font-size: 11px;
    font-weight: bold;
    cursor: help;
    position: relative;
    margin-left: 5px;
    vertical-align: middle;
  }
  .suite-help:hover { background:#666; color:#fff; }
  .suite-help .suite-tip {
    visibility: hidden;
    opacity: 0;
    transition: opacity .15s;
    position: absolute;
    left: 50%;
    top: 135%;
    transform: translateX(-50%);
    width: 270px;
    background: #333;
    color: #fff;
    font-size: 12px;
    font-weight: normal;
    line-height: 1.5;
    text-align: left;
    padding: 8px 10px;
    border-radius: 6px;
    z-index: 1000;
  }
  .suite-help:hover .suite-tip { visibility: visible; opacity: 1; }

  .suite-card {
    background: #f6f6f8;
    border-radius: 6px;
    padding: 10px 14px;
    margin-bottom: 12px;
  }
  .suite-card h4 { margin-top: 0; }
  .suite-muted { color: #666; font-size: 12px; }
  .suite-mono {
    font-family: 'Courier New', monospace;
    font-size: 13px;
    background: #f6f6f8;
    border-radius: 6px;
    padding: 10px 14px;
  }
  .suite-lock {
    background: #fafafa;
    border: 1px solid #e6e6e6;
    border-radius: 8px;
    padding: 12px 14px;
    color: #555;
    margin-bottom: 12px;
  }
  .suite-status {
    display: inline-block;
    border-radius: 999px;
    padding: 2px 8px;
    font-size: 11px;
    font-weight: 700;
    margin-left: 6px;
  }
  .suite-status-ready { background: #e8f5ef; color: #1D7F5C; }
  .suite-status-pending { background: #f4f4f4; color: #666; }
  .suite-status-warn { background: #fff6e6; color: #a05a00; }
  .suite-compact-status {
    display: flex;
    flex-wrap: wrap;
    gap: 8px 12px;
    align-items: center;
    padding: 6px 0 10px;
    color: #555;
    font-size: 12px;
    border-bottom: 1px solid #f1f1f1;
    margin-bottom: 10px;
  }
  .suite-compact-status strong { color: #333; }
  .suite-scroll-table {
    width: 100%;
    overflow-x: auto;
    overflow-y: auto;
    max-height: 420px;
    margin-bottom: 12px;
  }
  .suite-scroll-table table {
    min-width: 1200px;
    white-space: normal;
  }
  "
}

suite_help_label <- function(text, tip) {
  tags$span(
    text,
    tags$span(
      class = "suite-help", "?",
      tags$span(class = "suite-tip", tip)
    )
  )
}

suite_stage_notice <- function(title, text) {
  tags$div(
    class = "suite-card",
    h4(title),
    p(class = "suite-muted", text)
  )
}

suite_compact_status <- function(stage, status, detail, status_type = c("ready", "pending", "warn")) {
  status_type <- match.arg(status_type)
  tags$div(
    class = "suite-compact-status",
    tags$strong(stage),
    suite_status_pill(status, status_type),
    tags$span(detail)
  )
}

suite_status_pill <- function(text, status = c("ready", "pending", "warn")) {
  status <- match.arg(status)
  tags$span(class = paste("suite-status", paste0("suite-status-", status)), text)
}

suite_locked_notice <- function(stage, requirement) {
  tags$div(
    class = "suite-lock",
    h4(paste("Locked:", stage)),
    p(class = "suite-muted", requirement)
  )
}

suite_gate <- function(is_ready, stage, requirement, content) {
  if (isTRUE(is_ready)) {
    content
  } else {
    suite_locked_notice(stage, requirement)
  }
}

suite_placeholder_table <- function(rows) {
  tags$table(
    style = "width:100%; border-collapse: collapse;",
    lapply(rows, function(row) {
      tags$tr(lapply(row, function(cell) {
        tags$td(
          style = "border-bottom:1px solid #eee; padding:7px 8px;",
          cell
        )
      }))
    })
  )
}

suite_scroll_table <- function(output, max_height = "420px", min_width = "1200px") {
  tags$div(
    class = "suite-scroll-table",
    style = sprintf("max-height:%s;", max_height),
    tags$div(style = sprintf("min-width:%s;", min_width), output)
  )
}
