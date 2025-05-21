// import zio._
// import zio.Duration._ // 引入 Duration 相關的輔助方法，例如 .seconds

// object SleepExample extends ZIOAppDefault {

//   val myProgram: ZIO[Any, java.io.IOException, Unit] =
//     for {
//       _ <- Console.printLine("程式開始執行...")
//       startTime <- Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS) // 獲取當前時間 (毫秒)

//       _ <- Console.printLine("準備睡 2 秒鐘...")
//       _ <- ZIO.sleep(2.seconds) // 讓當前的 Fiber 睡眠 2 秒
//       _ <- Console.printLine("睡了 2 秒鐘。")

//       timeAfterFirstSleep <- Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
//       duration1 = (timeAfterFirstSleep - startTime) / 1000.0
//       _ <- Console.printLine(f"從開始到現在過了 $duration1%.2f 秒。")

//       _ <- Console.printLine("準備再睡 1 秒鐘...")
//       _ <- ZIO.sleep(1.second) // 也可以用 .second
//       _ <- Console.printLine("又睡了 1 秒鐘。")

//       endTime <- Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
//       duration2 = (endTime - startTime) / 1000.0
//       _ <- Console.printLine(f"程式結束。總共執行時間約為 $duration2%.2f 秒。")
//     } yield ()

//   // Clock.currentTime 需要 Clock 服務，ZIOAppDefault 預設提供了它
//   // 所以 myProgram 的 R 型別可以是 Any (或 Clock)
//   // Console.printLine 的 E 型別是 IOException
//   def run = myProgram
// }