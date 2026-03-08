#include <limits.h>
#include <stdbool.h>

#include <libfyaml.h>

static void fyz_silent_diag_output(struct fy_diag *diag, void *user,
                                   const char *buf, size_t len) {
  (void)diag;
  (void)user;
  (void)buf;
  (void)len;
}

struct fy_diag *fyz_create_silent_diag(void) {
  struct fy_diag_cfg cfg = {
      .fp = NULL,
      .output_fn = fyz_silent_diag_output,
      .user = NULL,
      .level = FYET_DEBUG,
      .module_mask = UINT_MAX,
      .colorize = false,
      .show_source = false,
      .show_position = false,
      .show_type = false,
      .show_module = false,
      .source_width = 0,
      .position_width = 0,
      .type_width = 0,
      .module_width = 0,
  };

  return fy_diag_create(&cfg);
}
