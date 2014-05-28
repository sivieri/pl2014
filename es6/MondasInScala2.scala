object WikipediaSuggest extends SimpleSwingApplication with ConcreteSwingApi with ConcreteWikipediaApi {
  
  /* GUI setup */
  def top = new MainFrame {

    val button = new Button("Get") {
      icon = new javax.swing.ImageIcon(javax.imageio.ImageIO.read(this.getClass.getResourceAsStream("/suggestions/wiki-icon.png")))
    }
    
    val suggestionList = new ListView(ListBuffer[String]())
    val status = new Label(" ")
    val editorpane = new EditorPane {
      import javax.swing.border._
      border = new EtchedBorder(EtchedBorder.LOWERED)
      editable = false
      peer.setContentType("text/html")
    }

    [...]

    /*
     * An Observable is a monad with support for three events: a new value is available, the stream is completed,
     * an error occurred. Since it is a monad, we can concatenate operations (e.g., filter, map, flatMap) and be
     * sure that if an error occurs, then it will be propagated in the stream.
     */
    val selections: Observable[String] = button.clicks.filter(_ => suggestionList.selection.indices.size != 0).map(_ => suggestionList.selection.items(0))
    val pages: Observable[Try[String]] = selections.flatMap(s => ObservableEx.apply(wikipediaPage(s))).recovered
    
    /*
     * This is where we listen only for the current values, and check if there has been an error somewhere.
     * Try is another variation of the Maybe monad, where there is the result of a computation or an exception.
     */
    val pageSubscription: Subscription = pages.observeOn(eventScheduler) subscribe {
      x =>
        x match {
          case Success(s) => editorpane.text = s
          case Failure(e) => status.text = e.getLocalizedMessage()
        }
    }

  }

}
