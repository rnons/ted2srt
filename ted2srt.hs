import Yesod
import Foundation
import Settings


main :: IO ()
main = do
    s <- staticSite
    warpDebug 3000 $ Ted s
