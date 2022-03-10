package choreo.view

/**
 * Created by   on 31/10/2020
 */

trait Dot[-A]:
  extension(e:A)
    def toDot:String
