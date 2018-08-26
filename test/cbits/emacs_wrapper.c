#include <emacs-module.h>

#include <stdlib.h>
#include "HsFFI.h"
#include "Rts.h"

#include <Emacs/TestsInit_stub.h>

int plugin_is_GPL_compatible = 1;

HsBool init(void) {
  int argc = 0;
  char *argv[] = { NULL };
  char **pargv = argv;

  // Initialize Haskell runtime
  {
      RtsConfig conf = defaultRtsConfig;
      conf.rts_opts_enabled = RtsOptsAll;
      hs_init_ghc(&argc, &pargv, conf);
  }
  return HS_BOOL_TRUE;
}

void deinit(void) {
  hs_exit();
}

int
emacs_module_init(struct emacs_runtime *ert)
{
  return !(init() && initialise(ert));
}
