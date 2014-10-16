As described in the QuickStart manual, "make external" will download all supported model codes and MOSSCO's Make system will automatically take care of their correct compilation. For users, who want to use their own versions of the model codes, various environment variables can be set to control their inclusion. Below the precedence of possible environment variables for the different models is shown.

FABM
====

1) MOSSCO_FABM_BINARY_DIR (build directory prepared with cmake for FABMHOST=mossco)
2) MOSSCO_FABM_PREFIX (directory with precompiled code for FABMHOST=mossco)
3) MOSSCO_FABMDIR (top-level directory of FABM)
4) external/fabm-git (created e.g. with "make external")
5) FABMDIR (top-level directory of FABM also used for GOTM/GETM)


GOTM
====

1) MOSSCO_GOTMDIR (top-level directory of GOTM MOSSCO is allowed to compile in)
2) external/gotm-git (created e.g. with "make external")
3) GOTMDIR (top-level directory of GOTM with precompiled code without FABM support)


GETM
====

1) MOSSCO_GETMDIR (top-level directory of GETM MOSSCO is allowed to compile in)
2) external/getm-git (created e.g. with "make external")
3) GETMDIR (top-level directory of GETM with precompiled code without FABM support)
