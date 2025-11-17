package org.aurora.sjsast


class ModulePCMTest extends BaseAsyncTest:

  "ModulePCM-0" should {

    "be a module" in { 
      //TODO note if there is no file named ArnoldModule-0.aurora, it will automatically create one
      //SHOULD TEST FILE NAMING BE MORE DESCRIPTIVE e.g. ArnoldModule-Valid.auror
      
      //TODO add a test that verifies a non-module using Either[L,R] instead of exception handling

      for {
        astPCM      <- parse(0)
        modulePCM   <- Future(ModulePCM(astPCM))
        assertName  <- modulePCM.module.name should be("chf_2024")
        _           <- finfo(s"${modulePCM.show}")
      } yield assertName
    } 
  }
