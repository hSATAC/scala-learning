// 資料模型
case class Stock(symbol: String, name: String, price: Double, change: Double, outstandingShares: Long)
case class PriceData(date: String, open: Double, high: Double, low: Double, close: Double, volume: Long)

// 分析功能
object StockAnalyzer:
  // 1. 計算股票的市值（股價 * 流通股數）
  // def marketCap(stock: Stock): Double =
  //   stock.price * stock.outstandingShares

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

  // 1. 定義過濾器和轉換器類型
  type StockFilter = Stock => Boolean
  type StockTransformer = Stock => Double

  // 2. 組合兩個過濾器（兩個條件都必須滿足）
  def andFilter(filter1: StockFilter, filter2: StockFilter): StockFilter =
    stock => filter1(stock) && filter2(stock)

  // 3. 組合兩個過濾器（至少一個條件滿足）
  def orFilter(filter1: StockFilter, filter2: StockFilter): StockFilter =
    stock => filter1(stock) || filter2(stock)

  // 4. 組合兩個轉換器（先用第一個，再將結果傳給第二個）
  def composeTransformers(t1: StockTransformer, t2: Double => Double): StockTransformer =
    stock => t2(t1(stock))

  // 5. 使用過濾器和轉換器的函數
  def filterAndTransform(stocks: List[Stock], filter: StockFilter, transformer: StockTransformer): List[Double] =
    stocks.filter(filter).map(transformer)

  // 1. 定義不同類型的金融資產 
  enum Asset:
    case StockAsset(stock: Stock)
    case BondAsset(name: String, faceValue: Double, couponRate: Double)
    case CashAsset(amount: Double, currency: String)

  // 2. 計算任何資產的價值
  def assetValue(asset: Asset): Double =
    asset match
      case Asset.StockAsset(stock) => stock.price * stock.outstandingShares
      case Asset.BondAsset(_, faceValue, _) => faceValue
      case Asset.CashAsset(amount, _) => amount

  // 3. 計算投資組合的總價值
  def portfolioValue(assets: List[Asset]): Double =
    assets.map(assetValue).sum

  // 4. 根據資產類型進行分類
  def categorizeAssets(assets: List[Asset]): Map[String, List[Asset]] =
    assets.groupBy(asset => asset match
      case Asset.StockAsset(stock) => "Stock"
      case Asset.BondAsset(name, _, _) => "Bond"
      case Asset.CashAsset(amount, currency) => "Cash"
    )
      
  // 使用 Scala 3 的 extension methods 為現有類型添加功能

  // 1. 為 Stock 添加計算市值、類別等方法
  extension (stock: Stock)
    def marketCap: Double = stock.price * stock.outstandingShares
    def priceToBookRatio(bookValuePerShare: Double): Double = stock.price / bookValuePerShare
    def category: String =
      if stock.outstandingShares > 1000000000 then "Large Cap"
      else if stock.outstandingShares > 500000000 then "Mid Cap"
      else "Small Cap"

  // 2. 為 List[Stock] 添加有用的分析方法
  extension (stocks: List[Stock])
    def averagePrice: Double =
      if stocks.isEmpty then 0.0 
      else stocks.map(_.price).sum / stocks.length
    def totalMarketCap: Double = stocks.map(_.marketCap).sum
    def highestPriceStock: Option[Stock] = stocks.maxByOption(_.price)
    def sortByMarketCap: List[Stock] = stocks.sortBy(_.marketCap)

  // 3. 為 List[PriceData] 添加技術分析方法
  extension (priceData: List[PriceData])
    def calculateRSI(period: Int): List[Double] =
      // 確保數據足夠
      if priceData.length <= period + 1 then return List()
      
      // 計算價格變動（使用收盤價）
      val closePrices = priceData.map(_.close)
      val changes = closePrices.zip(closePrices.tail).map { case (prev, curr) => curr - prev }
      
      // 計算窗口內的 RSI 值
      changes.sliding(period).map { windowChanges =>
        // 分離正變動和負變動
        val gains = windowChanges.filter(_ > 0).sum
        val losses = windowChanges.filter(_ < 0).map(math.abs).sum
        
        // 計算相對強度和 RSI
        if losses == 0 then 100.0  // 避免除以零
        else {
          val rs = gains / losses
          100.0 - (100.0 / (1.0 + rs))
        }
      }.toList
    
    private def calculateEMA(prices: List[Double], period: Int): List[Double] =
      if prices.length < period then return List()
      
      // 簡化：前 period 個值使用簡單移動平均作為起始 EMA
      val firstEMA = prices.take(period).sum / period
      
      // 計算係數
      val multiplier = 2.0 / (period + 1)
      
      // 計算剩餘的 EMA 值
      prices.drop(period).scanLeft(firstEMA) { (prevEMA, price) =>
        (price - prevEMA) * multiplier + prevEMA
      }
    
    def calculateMACD(
      fastPeriod: Int = 12, 
      slowPeriod: Int = 26, 
      signalPeriod: Int = 9
    ): (List[Double], List[Double], List[Double]) =
      // 獲取收盤價
      val closingPrices = priceData.map(_.close)
      
      // 計算快速和慢速 EMA
      val fastEMA = calculateEMA(closingPrices, fastPeriod)
      val slowEMA = calculateEMA(closingPrices, slowPeriod)
      
      // 確保長度相同
      if fastEMA.isEmpty || slowEMA.isEmpty then 
        return (List(), List(), List())
      
      // 計算 MACD 線
      val macdLine = fastEMA.zip(slowEMA).map { case (fast, slow) => fast - slow }
      
      // 計算信號線 (MACD 的 EMA)
      val signalLine = calculateEMA(macdLine, signalPeriod)
      
      // 計算柱狀圖 (MACD - 信號線)
      val histogram = macdLine.zip(signalLine).map { case (macd, signal) => macd - signal }
      
      (macdLine, signalLine, histogram)

  // 過濾上漲的股票
  def gainers(stocks: List[Stock]): List[Stock] = 
    stocks.filter(stock => stock.change > 0)
  
  // 過濾下跌的股票
  def losers(stocks: List[Stock]): List[Stock] = 
    stocks.filter(stock => stock.change < 0)
  
  // 計算平均價格
  // def averagePrice(stocks: List[Stock]): Double = 
  //   if stocks.isEmpty then 0.0 
  //   else stocks.map(_.price).sum / stocks.length
  
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

  val priceData = List(
    PriceData("2023-01-01", 100.0, 105.0, 98.0, 103.0, 1000000),
    PriceData("2023-01-02", 103.0, 107.0, 102.0, 106.0, 1100000),
    PriceData("2023-01-03", 106.0, 108.0, 104.0, 105.0, 900000),
    PriceData("2023-01-04", 105.0, 106.0, 101.0, 102.0, 1200000),
    PriceData("2023-01-05", 102.0, 104.0, 100.0, 101.0, 800000),
    PriceData("2023-01-06", 101.0, 103.0, 99.0, 102.0, 900000),
    PriceData("2023-01-07", 102.0, 105.0, 101.0, 104.0, 1000000),
    PriceData("2023-01-08", 104.0, 108.0, 103.0, 107.0, 1300000),
    PriceData("2023-01-09", 107.0, 110.0, 106.0, 109.0, 1400000),
    PriceData("2023-01-10", 109.0, 112.0, 108.0, 110.0, 1200000),
    PriceData("2023-01-11", 110.0, 113.0, 109.0, 112.0, 1100000),
    PriceData("2023-01-12", 112.0, 115.0, 111.0, 114.0, 1300000),
    PriceData("2023-01-13", 114.0, 116.0, 112.0, 113.0, 1000000),
    PriceData("2023-01-14", 113.0, 114.0, 110.0, 111.0, 900000),
    PriceData("2023-01-15", 111.0, 112.0, 109.0, 110.0, 800000),
    PriceData("2023-01-16", 110.0, 111.0, 108.0, 109.0, 700000),
    PriceData("2023-01-17", 109.0, 112.0, 108.0, 111.0, 900000),
    PriceData("2023-01-18", 111.0, 114.0, 110.0, 113.0, 1100000),
    PriceData("2023-01-19", 113.0, 116.0, 112.0, 115.0, 1300000),
    PriceData("2023-01-20", 115.0, 118.0, 114.0, 117.0, 1400000),
    PriceData("2023-01-21", 117.0, 120.0, 116.0, 119.0, 1500000),
    PriceData("2023-01-22", 119.0, 122.0, 118.0, 121.0, 1600000),
    PriceData("2023-01-23", 121.0, 123.0, 119.0, 120.0, 1400000),
    PriceData("2023-01-24", 120.0, 121.0, 117.0, 118.0, 1200000),
    PriceData("2023-01-25", 118.0, 119.0, 115.0, 116.0, 1100000),
    PriceData("2023-01-26", 116.0, 118.0, 114.0, 117.0, 1300000),
    PriceData("2023-01-27", 117.0, 120.0, 116.0, 119.0, 1400000),
    PriceData("2023-01-28", 119.0, 122.0, 118.0, 121.0, 1500000),
    PriceData("2023-01-29", 121.0, 124.0, 120.0, 123.0, 1600000),
    PriceData("2023-01-30", 123.0, 126.0, 122.0, 125.0, 1700000),
    PriceData("2023-01-31", 125.0, 128.0, 124.0, 127.0, 1800000),
    PriceData("2023-02-01", 127.0, 130.0, 126.0, 129.0, 1900000),
    PriceData("2023-02-02", 129.0, 132.0, 128.0, 131.0, 2000000),
    PriceData("2023-02-03", 131.0, 133.0, 129.0, 130.0, 1800000),
    PriceData("2023-02-04", 130.0, 131.0, 127.0, 128.0, 1600000),
    PriceData("2023-02-05", 128.0, 129.0, 125.0, 126.0, 1400000),
    PriceData("2023-02-06", 126.0, 128.0, 124.0, 127.0, 1500000),
    PriceData("2023-02-07", 127.0, 130.0, 126.0, 129.0, 1700000),
    PriceData("2023-02-08", 129.0, 132.0, 128.0, 131.0, 1900000),
    PriceData("2023-02-09", 131.0, 134.0, 130.0, 133.0, 2100000)
  )

  // 2. 計算 RSI（使用14天週期）
  val rsiValues = priceData.calculateRSI(14)
  
  // 3. 計算 MACD
  val (macdLine, signalLine, histogram) = priceData.calculateMACD()
  
  // 4. 輸出結果
  println("=== 技術指標分析結果 ===")
  
  // 4.1 RSI 結果
  println("\n== RSI (14日) 值 ==")
  if rsiValues.isEmpty then
    println("數據不足，無法計算 RSI")
  else
    rsiValues.zip(priceData.drop(15)).foreach { case (rsi, data) =>
      println(f"${data.date}: RSI = ${rsi}%.2f")
    }
  
  // 4.2 MACD 結果
  println("\n== MACD 指標 ==")
  if macdLine.isEmpty then
    println("數據不足，無法計算 MACD")
  else
    // 由於 MACD 計算涉及多個期間，可能需要調整數據匹配
    val dates = priceData.drop(priceData.length - macdLine.length).map(_.date)
    
    println("日期       | MACD線  | 信號線  | 柱狀圖")
    println("----------|---------|---------|--------")
    
    macdLine.zip(signalLine).zip(histogram).zip(dates).foreach { 
      case (((macd, signal), hist), date) =>
        println(f"$date | ${macd}%7.2f | ${signal}%7.2f | ${hist}%7.2f")
    }
  
  // 5. 簡單視覺化（ASCII圖表）
  println("\n== RSI 視覺化 ==")
  if rsiValues.nonEmpty then
    visualizeValues(rsiValues, 0, 100)
  
  println("\n== MACD 視覺化 ==")
  if macdLine.nonEmpty then
    println("MACD線:")
    visualizeValues(macdLine, macdLine.min, macdLine.max)
    
    println("\n信號線:")
    visualizeValues(signalLine, signalLine.min, signalLine.max)

// 簡單的 ASCII 圖表視覺化
def visualizeValues(values: List[Double], min: Double, max: Double): Unit =
  val width = 50  // 圖表寬度
  val range = max - min
  
  values.foreach { value =>
    val position = if range == 0 then 0 else ((value - min) / range * width).toInt
    val bar = "=" * position + "|" + " " * (width - position)
    println(f"$value%7.2f $bar")
  }

  // 定義基本過濾器
  // val highPriceFilter: StockFilter = stock => stock.price > 500
  // val positiveChangeFilter: StockFilter = stock => stock.change > 0

  // // 組合過濾器
  // val highPriceAndPositiveChangeFilter = andFilter(highPriceFilter, positiveChangeFilter)

  // // 使用組合後的過濾器
  // val expensiveGainers = stocks.filter(highPriceAndPositiveChangeFilter)
  // println(expensiveGainers)

  // println(highestMarketCap(stocks))
  // println(calculatePE(stocks.head, 10.0))
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