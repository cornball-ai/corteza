#!/usr/bin/env r
#
# Estimate the token footprint of the LLM `tools` payload produced by
# schema_from_registry() in different environments.
#
# Token count uses the industry-standard approximation of ~4 chars per
# token for English / JSON content. Good enough for this comparison.

suppressMessages(library(corteza))
corteza::ensure_skills()

est_tokens <- function(schemas) {
    json <- jsonlite::toJSON(schemas, auto_unbox = TRUE)
    ceiling(nchar(json) / 4)
}

report <- function(label) {
    schemas <- corteza::schema_from_registry()
    cat(sprintf("%-40s  %2d tools   ~%5d tokens\n",
                label, length(schemas), est_tokens(schemas)))
}

# Measure in different environments to exercise Phase 6 pruning.

orig_wd <- getwd()
orig_tavily <- Sys.getenv("TAVILY_API_KEY", unset = NA)
on.exit({
    setwd(orig_wd)
    if (is.na(orig_tavily)) {
        Sys.unsetenv("TAVILY_API_KEY")
    } else {
        Sys.setenv(TAVILY_API_KEY = orig_tavily)
    }
}, add = TRUE)

cat("system-prompt tools payload:\n\n")

Sys.unsetenv("TAVILY_API_KEY")
tmp_nongit <- tempfile("nongit_")
dir.create(tmp_nongit)
setwd(tmp_nongit)
report("non-git dir, no TAVILY_API_KEY")

setwd(orig_wd)
unlink(tmp_nongit, recursive = TRUE)

tmp_git <- tempfile("git_")
dir.create(tmp_git)
setwd(tmp_git)
suppressWarnings(system2("git", c("init", "-q", "."),
                         stdout = FALSE, stderr = FALSE))
report("git repo, no TAVILY_API_KEY")
Sys.setenv(TAVILY_API_KEY = "tvly-fake")
report("git repo, TAVILY_API_KEY set")
Sys.unsetenv("TAVILY_API_KEY")

setwd(orig_wd)
unlink(tmp_git, recursive = TRUE)

# Unpruned baseline (every registered skill, ignoring available() gates).
cat("\nunpruned baseline (ignores available() predicates):\n")
all_skills <- corteza:::list_skills()
skills <- lapply(all_skills, corteza:::get_skill)
# Convert to the same shape schema_from_registry produces.
unpruned <- lapply(skills, function(s) {
    list(name = s$name,
         description = s$description,
         input_schema = s$inputSchema)
})
cat(sprintf("%-40s  %2d tools   ~%5d tokens\n",
            "all registered tools",
            length(unpruned), est_tokens(unpruned)))
