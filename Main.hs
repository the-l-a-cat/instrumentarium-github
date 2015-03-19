import Data.Maybe
import Control.Monad
import Data.Tree.NTree.TypeDefs
import Data.Tree.Class

import Github.Auth
import Github.Repos
import Github.Organizations
import Github.Users


main = do
            name <- getLine
            (liftM (either (\x -> "") (\x -> unlines $ map repoName x)) (Github.Repos.organizationRepos "pink-ink-inc"))
            >>= putStrLn

isOrganization name = isNotError_ $ publicOrganization name
isUser name = isNotError_ $ userInfoFor name

scanOrganizationRepos org = e_ (\x -> unlines $ map repoName x) (organizationRepos org)
scanUserRepos org = e_ (\x -> unlines $ map repoName x) (userRepos org All)

e_ :: Monad m => (o -> String) -> m (Either Error o) -> m String
e_ f m = liftM (either showError f) m
    where showError error = "Error" ++ show error

isNotError_ :: (Monad m) => m (Either Error o) -> m Bool
isNotError_ ret = liftM (either (\x->False) (\x->True)) ret


strategy :: o -> NTree (o -> Bool, Maybe (o -> p)) -> [p]
strategy obj (NTree (cond, func) children)
    | cond obj = maybe proceed (\f -> f obj : proceed) func
    | otherwise = []
                  where proceed = concat $ map (strategy obj) children

