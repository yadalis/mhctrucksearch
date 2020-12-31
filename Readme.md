
# Elm App

This project is bootstrapped with [Create Elm App](https://github.com/halfzebra/create-elm-app).

# Website

         This website was built on Elm (frontend platform) and APIs are built in .NET/C# to serve data.

# Purpose
 
         Truck Sales team needed a super fast search/fitler trucks to give the results instantly.

# Why Elm and .NET

         **Front-end** : Elm, like many others, I honeslty believe this is the best open-source/front-end tech out there in the market, looked at React.js, Angular and Vue, none could gave me the confidence that Elm did, so there is not much to make a decision on what to choose for frontend other than Elm, simply because of its super smart compiler with 100% type-safty, immutability, super fast Virtual DOM and most importantly, confident refactoring.
         
         Back-end: .NET/C#, this is what my primary skillset is, so there is not other thought on this side, simple.

# Packages used

         Elm-UI (100% elm-ui , no CSS other than just in-line style for sticky hearder)
         List-Extra
         Maybe-Extra
         elm-money
         json-extra
         unique-list

# Artifacts
         Url:
                  UI: https://mhctrucksmartsearch.azurewebsites.net/
                  
                  Data api endpoints:
                           https://testfuncappsuresh.azurewebsites.net/api/getusedtrucks
                           https://testfuncappsuresh.azurewebsites.net/api/getnewtrucks
                           https://testfuncappsuresh.azurewebsites.net/api/getappraisedtrucks

# When security errors, while runn elm app, try "set-executionpolicy remotesigned" in powershell (as admin)


# To run with trucksdata.json file within the project, just do the following
         first, install create-elm-app NPM package
         install json-server using the command "npm install json-server -g"
         then, run, the command "json-server --watch src\truckdata.json --port 3004"
         return ""http://localhost:3004/trks"" as a url from fetchTrucksUrl & fetchAppraisedTrucksUrl functions in Commands.elm file
         the, jus run the "elm-app start"
         









