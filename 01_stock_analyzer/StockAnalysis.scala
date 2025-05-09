// 資料模型
case class Stock(symbol: String, name: String, price: Double, change: Double, outstandingShares: Long)
case class PriceData(date: String, open: Double, high: Double, low: Double, close: Double, volume: Long)

// 分析功能
object StockAnalyzer:
  // 1. 計算股票的市值（股價 * 流通股數）
  def marketCap(stock: Stock): Double =
    stock.price * stock.outstandingShares

  // 2. 找出市值最高的股票
  def highestMarketCap(stocks: List[Stock]): Option[Stock] =
    stocks.maxByOption(marketCap(_))

  // 3. 計算股票的本益比（P/E Ratio）：股價 / 每股盈餘
  // 假設每股盈餘(EPS)是個單獨的參數
  def calculatePE(stock: Stock, eps: Double): Option[Double] =
    Option(eps).filter(_ != 0).map(e => stock.price / e)

  // 1. 安全地計算兩支股票的價格比率，處理可能的零價格
  def priceRatio(stock1: Stock, stock2: Stock): Option[Double] =
    Option.when(stock1.price != 0 && stock2.price != 0)(stock1.price / stock2.price)

  // 2. 找出特定日期範圍內的最高和最低價格，若範圍內沒有資料則返回 None
  def findPriceRange(history: List[PriceData], startDate: String, endDate: String): Option[(Double, Double)] =
    val inRange = history.filter(data => data.date >= startDate && data.date <= endDate)
    for {
      highest <- inRange.map(_.high).maxOption
      lowest <- inRange.map(_.low).minOption
    } yield (highest, lowest)

  // 3. 嘗試找出某個股票代號的資訊，若不存在則返回 None
  def findStock(stocks: List[Stock], symbol: String): Option[Stock] =
    stocks.find(_.symbol == symbol)

  // 過濾上漲的股票
  def gainers(stocks: List[Stock]): List[Stock] = 
    stocks.filter(stock => stock.change > 0)
  
  // 過濾下跌的股票
  def losers(stocks: List[Stock]): List[Stock] = 
    stocks.filter(stock => stock.change < 0)
  
  // 計算平均價格
  def averagePrice(stocks: List[Stock]): Double = 
    if stocks.isEmpty then 0.0 
    else stocks.map(_.price).sum / stocks.length
  
  // 找出價格最高的股票
  def highestPrice(stocks: List[Stock]): Option[Stock] = 
    stocks.maxByOption(_.price)
  
  // 計算股票漲跌幅
  def priceChangePercentage(stock: Stock): Double = 
    (stock.change / (stock.price - stock.change)) * 100
  
  // 用漲跌幅排序股票
  def sortByChangePercentage(stocks: List[Stock]): List[Stock] = 
    stocks.sortBy(stock => -priceChangePercentage(stock))
  
  // 計算歷史資料的平均交易量
  def averageVolume(history: List[PriceData]): Double = 
    if history.isEmpty then 0.0 
    else history.map(_.volume).sum.toDouble / history.length
  
  // 計算歷史價格的波動率 (高價與低價的差異)
  def priceVolatility(history: List[PriceData]): List[Double] = 
    history.map(data => data.high - data.low)
  
  // 根據收盤價計算簡單移動平均線 (SMA)
  def calculateSMA(history: List[PriceData], days: Int): List[Double] = 
    if history.length < days then List()
    else 
      history
        .map(_.close)
        .sliding(days)
        .map(window => window.sum / days)
        .toList

// 主程式
@main def runStockAnalysis(): Unit =
  import StockAnalyzer.*
  
  // 創建一些範例資料
  val stocks = List(
    Stock("2330", "台積電", 825.0, 15.0, 1000000000),
    Stock("2454", "聯發科", 1150.0, -5.0, 1000000000),
    Stock("2317", "鴻海", 142.5, 1.5, 1000000000),
    Stock("1301", "台塑", 78.2, -0.8, 1000000000),
    Stock("2412", "中華電", 126.5, 0.5, 1000000000)
  )

  // 假設我們有台積電的一些歷史資料
  val tsmcHistory = List(
    PriceData("2025-05-08", 810.0, 826.0, 810.0, 825.0, 25000000),
    PriceData("2025-05-07", 805.0, 812.0, 800.0, 810.0, 22000000),
    PriceData("2025-05-06", 800.0, 808.0, 798.0, 805.0, 20000000),
    PriceData("2025-05-05", 790.0, 802.0, 790.0, 800.0, 18000000),
    PriceData("2025-05-04", 785.0, 795.0, 785.0, 790.0, 15000000)
  )
  println(highestMarketCap(stocks))
  println(calculatePE(stocks.head, 10.0))
//   println("上漲的股票:")
//   gainers(stocks).foreach(s => println(s"${s.name}: ${s.change}"))
  
//   println("\n下跌的股票:")
//   losers(stocks).foreach(s => println(s"${s.name}: ${s.change}"))
  
//   println(s"\n所有股票的平均價格: ${averagePrice(stocks)}")
  
//   highestPrice(stocks) match
//     case Some(stock) => println(s"\n價格最高的股票是: ${stock.name}, 價格: ${stock.price}")
//     case None => println("\n沒有找到股票")
  
//   println("\n按漲跌幅排序的股票:")
//   sortByChangePercentage(stocks).foreach(s => 
//     println(f"${s.name}: ${priceChangePercentage(s)}%.2f%%")
//   )
  
//   println(s"\n台積電平均交易量: ${averageVolume(tsmcHistory)}")
  
//   println("\n台積電價格波動:")
//   priceVolatility(tsmcHistory).zip(tsmcHistory).foreach { 
//     case (volatility, data) => println(f"${data.date}: ${volatility}%.2f")
//   }
  
//   println("\n台積電3日移動平均線:")
//   val sma = calculateSMA(tsmcHistory, 3)
//   if sma.nonEmpty then
//     sma.zip(tsmcHistory.dropRight(2)).foreach {
//       case (avg, data) => println(f"${data.date}: ${avg}%.2f")
//     }
//   else
//     println("資料不足，無法計算移動平均線")