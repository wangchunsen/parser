// package parser

// import org.scalatest.FreeSpec
// import parser.doc.Element

// class HtmlParser extends FreeSpec{
//   "Shoud parser element" in {
//     val html =
//       """<link rel='stylesheet' id='wp-block-library-css'
//         |		href='http://www.runoob.com/wp-includes/css/dist/block-library/style.min.css?ver=5.0.3' type='text/css'
//         |		media='all' />
//       """.stripMargin
//     val value = Parser.run(html, MyParser.element)
//     println(value)
//   }

//   "Self close elment" in {
//     val value = Parser.run("<br />", MyParser.element)
//     assert(Res.toOption(value).contains(Element("br")))
//   }

//   "parser element" in {
//     val html =
//       """<div class="sidebar-box ad-box ad-box-large">
//         |						<div id="ad-336280">
//         |             <script async src="//pagead2.googlesyndication.com/pagead/js/adsbygoogle.js"></script>
//         |							<!-- 移动版 自动调整 -->
//         |							<ins class="adsbygoogle" style="display:block" data-ad-client="ca-pub-5751451760833794"
//         |								data-ad-slot="1691338467" data-ad-format="auto"></ins>
//         |							<script>
//         |								(adsbygoogle = window.adsbygoogle || []).push({});
//         |							</script>
//         |						</div>
//         |					</div>
//       """.stripMargin

//     val value = Parser.run(html, MyParser.element)
//     assert(Res.toOption(value).isDefined)
//   }

//   "parser html" in {
//     val html =
//       """<div class="dbs" style="display:none;margin-left:15px;"><a href="https://app.zblogcn.com/11.php"
//         |				target="_blank"><img src="/style/images/app11.png" width="960" alt="应用中心“双11”活动"
//         |					title=应用中心“双11”活动" /></a>
//         |		</div>
//       """.stripMargin
//     val value = Parser.run(html, MyParser.element)
//     println(value)
//     assert(Res.toOption(value).isDefined)
//   }
// }
