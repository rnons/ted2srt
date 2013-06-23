import Yesod
import Foundation
import Settings


main :: IO ()
main = do
    s <- staticSite
    warp 3000 $ Ted s
