package org.aurora.sjsast


class PCMIdempotencyTest extends BaseAsyncTest:

  "PCMIdempotencyTest" should {
    "be a module" in { 


      for {
        tspcm0      <- parse(0)
        tspcm1     <- parse(1)
        pcm0         <- Future(PCM(tspcm0))
        pcm1     <- Future(PCM(tspcm1))
        _           <- pcm0 should be(pcm1)
        _           <-  (pcm0 |+| pcm0) should be(pcm0)
        _           <- pcm0.module should be (None)
        a           <- (true should be(true))
        _           <- finfo(s"${pcm0.show}")
      } yield {
         a
      }    } 
  }