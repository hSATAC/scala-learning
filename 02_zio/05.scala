// import zio._
// import zio.Duration._ // 為了 .seconds, .millisecond 等

// object SequentialExecutionExample extends ZIOAppDefault {

//   // 效果 A：模擬一個耗時 2 秒的操作
//   val effectA: ZIO[Any, Nothing, String] =
//     ZIO.succeed("結果 A").delay(2.seconds) <* Console.printLine("效果 A 完成").orDie
//     // .delay(duration) 是 ZIO.sleep(duration) *> ZIO.succeed(value_from_original_effect) 的簡寫
//     // <* Console.printLine(...) 表示執行 Console.printLine，但保留 ZIO.succeed(...).delay(...) 的結果

//   // 效果 B：模擬一個耗時 1 秒的操作
//   val effectB: ZIO[Any, Nothing, String] =
//     ZIO.succeed("結果 B").delay(1.second) <* Console.printLine("效果 B 完成").orDie

//   val sequentialProgram = for {
//     _         <- Console.printLine("開始序列執行...").orDie
//     startTime <- Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
//     resultA   <- effectA
//     resultB   <- effectB
//     endTime   <- Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
//     duration  = (endTime - startTime).millis
//     _         <- Console.printLine(s"所有效果完成。結果: ($resultA, $resultB)。總耗時: ${duration.render}").orDie
//   } yield ()

//   def run = sequentialProgram
// }
// import zio._
// import zio.Duration._

// object ParallelExecutionZipPar extends ZIOAppDefault {

//   val effectA: ZIO[Any, Nothing, String] =
//     ZIO.succeed("結果 A (平行)").delay(2.seconds) <* Console.printLine("效果 A (平行) 完成").orDie

//   val effectB: ZIO[Any, Nothing, String] =
//     ZIO.succeed("結果 B (平行)").delay(1.second) <* Console.printLine("效果 B (平行) 完成").orDie

//   val parallelProgram = for {
//     _         <- Console.printLine("開始平行執行 (zipPar)...").orDie
//     startTime <- Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
//     // 使用 zipPar 平行執行 effectA 和 effectB
//     tupleResult <- effectA.zipPar(effectB) // tupleResult 會是 (String, String)
//     endTime     <- Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
//     duration    = (endTime - startTime).millis
//     _           <- Console.printLine(s"所有效果 (平行) 完成。結果: $tupleResult。總耗時: ${duration.render}").orDie
//   } yield ()

//   def run = parallelProgram
// }
import zio._
import zio.Duration._

object ParallelForeachExample extends ZIOAppDefault {

  val tasks = List("任務1", "任務2", "任務3", "任務4")

  // 模擬處理單個任務的函式，它回傳一個 ZIO 效果
  def processTask(taskName: String): ZIO[Any, Nothing, String] = {
    val processingTime = (scala.util.Random.nextInt(3) + 1).seconds // 隨機 1-3 秒
    Console.printLine(s"開始處理 $taskName (預計耗時 ${processingTime.render})").orDie *>
    ZIO.succeed(s"$taskName 已處理完成")
      .delay(processingTime) <* Console.printLine(s"$taskName 完成處理").orDie
  }

  val parallelTaskProcessing = for {
    _         <- Console.printLine(s"開始平行處理 ${tasks.size} 個任務 (foreachPar)...").orDie
    startTime <- Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
    // 使用 foreachPar 平行處理所有任務
    results   <- ZIO.foreachPar(tasks)(taskName => processTask(taskName)) // results 會是 List[String]
    endTime   <- Clock.currentTime(java.util.concurrent.TimeUnit.MILLISECONDS)
    duration  = (endTime - startTime).millis
    _         <- Console.printLine(s"所有任務 (平行) 完成。結果: $results。總耗時: ${duration.render}").orDie
  } yield ()

  def run = parallelTaskProcessing
}