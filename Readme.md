
# Elm App

This project is bootstrapped with [Create Elm App](https://github.com/halfzebra/create-elm-app).
This project has been changed to use Vite as a build manager and used Vite-Plugin-Elm for watching changes automatically and refresh the page

# Website

         This website was built on Elm (frontend platform) and APIs are built in .NET/C# to serve data.
         When you donwload and test the code, it uses the json-server to host the data it needed to run, however, you can replace
         the URLs with the real APIs hosted.

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

# Artifacts - Not valid anymore
         Url:
                  UI: https://mhctrucksmartsearch.azurewebsites.net/
                  
                  Data api endpoints:
                           https://testfuncappsuresh.azurewebsites.net/api/getusedtrucks
                           https://testfuncappsuresh.azurewebsites.net/api/getnewtrucks
                           https://testfuncappsuresh.azurewebsites.net/api/getappraisedtrucks

# When security errors, while runn elm app, try "set-executionpolicy remotesigned" in powershell (as admin)


# To run with trucksdata.json file within the project, just do the following
         Once you clone this project, Make sure you have installed Vite and Vite-Plugin-Elm packages.
         install json-server using the command "npm install json-server -g"
         
         then, run, the command "json-server --watch src\newtruckdata.json --port 3004" (any truck data in a json file should contain a root elemnts named "trks" (http://localhost:3004/trks)

         then, run the command "json-server --watch src\srcFilterRanges.json --port 3005" (http://localhost:3005/srchRanges)
         
         return ""http://localhost:3004/trks"" as a url from fetchTrucksUrl & fetchAppraisedTrucksUrl functions in Commands.elm file
         then, just run the "npm run dev"
         









