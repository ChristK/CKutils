# Tests for get_dropbox_path() on the non-Windows (Linux/macOS) code path.
# get_dropbox_path() reads "~/.dropbox/info.json". To avoid clobbering a real
# Dropbox install, we only run when that directory does not already exist, and
# we always clean up what we create.

dropbox_dir <- path.expand("~/.dropbox")
info_json   <- file.path(dropbox_dir, "info.json")

if (.Platform$OS.type == "windows") {
  exit_file("get_dropbox_path Linux-path tests skipped on Windows")
}
if (file.exists(dropbox_dir)) {
  exit_file("~/.dropbox already exists - skip to avoid clobbering a real install")
}

dir.create(dropbox_dir)

# --- personal + business paths present --------------------------------------
writeLines(
  jsonlite::toJSON(
    list(personal = list(path = "/tmp/ck_dropbox_personal"),
         business = list(path = "/tmp/ck_dropbox_business")),
    auto_unbox = TRUE),
  info_json)

expect_equal(get_dropbox_path(),
             normalizePath("/tmp/ck_dropbox_personal/", mustWork = FALSE),
             info = "get_dropbox_path personal (default)")
expect_equal(get_dropbox_path(type = "business"),
             normalizePath("/tmp/ck_dropbox_business/", mustWork = FALSE),
             info = "get_dropbox_path business")
# pathtail is appended
expect_equal(get_dropbox_path("sub/dir"),
             normalizePath("/tmp/ck_dropbox_personal/sub/dir", mustWork = FALSE),
             info = "get_dropbox_path appends pathtail")

# --- info.json present but no path field -> 'cannot be located' error --------
writeLines(jsonlite::toJSON(list(other = list(x = 1)), auto_unbox = TRUE),
           info_json)
expect_error(get_dropbox_path(),
             pattern = "cannot be located",
             info = "get_dropbox_path errors when no path is recorded")

# cleanup (tinytest evaluates top-level expressions individually, so on.exit()
# would fire too early; remove the scratch directory explicitly at the end).
unlink(dropbox_dir, recursive = TRUE)
