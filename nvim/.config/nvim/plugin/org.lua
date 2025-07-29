-- Setup orgmode
local org = require("orgmode")
org.setup({
  org_agenda_files = "~/Documents/wiki/**/*",
  org_default_notes_file = "~/Documents/wiki/notes.org",
  org_todo_keywords = { "TODO(t!)", "PROG(p!)", "BLOCKED(b!)", "HOLD(h!)", "REVIEW(r!)", "|", "DONE(d!)" },
  org_todo_keyword_faces = {
    ["TODO"] = "foreground #4CAF50",
    ["PROG"] = "foreground #ff9800",
    ["BLOCKED"] = "foreground #F44336",
    ["REVIEW"] = "foreground #9C27B0",
    ["HOLD"] = "foreground #F44336",
    ["IDEA"] = "foreground #9C27B0",
    ["DONE"] = "foreground white",
  },
  org_capture_templates = {
    t = { description = "Todo", target = "~/Documents/wiki/todo.org", template = "* TODO [#%^{A|B|C}] %? %t" },
    j = {
      description = "Journal",
      target = "~/Documents/wiki/journal.org",
      template = "%?",
      datetree = true
    },
    f = { description = "File Context", template = "** %^{header}\n*** %a\n%?", headline = "Working notes" },
  },
  mappings = {
    global = {
      org_capture = "<leader>X",
    },
  },
})
