object SiteListings extends Controller {
    [...]

    /*
     * This example works exactly like the Maybe monad in Haskell:
     * each method in the model class Categories returns an Option,
     * which may contain the result of the db lookup or nothing.
     * We compose these values using map and flatMap (flatMap == bind),
     * and we return the resulting Web page created with Ok(). If
     * one of the lookups fail, the subsequent lookups are not executed
     * and the computation jumps to the getOrElse(), which checks if
     * there is an error and if so it returns a 404 code.
     */
    def listSensors(categoryId: Long) = DBAction { implicit request =>
        Categories.findById(categoryId).flatMap { category =>
            Categories.findSite(categoryId).map { site =>
                Ok(views.html.sensors(site, category, Sensors.findByCategory(categoryId)))
            }
        }.getOrElse(NotFound)
    }

}
