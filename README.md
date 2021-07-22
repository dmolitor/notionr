# notionr
Minimal client to access the [Notion API - Beta](https://developers.notion.com/docs/getting-started).

## Installation
Get the development version from github with:
```r
# install.packages("devtools")
devtools::install_github("dmolitor/notionr")
```
## Setup
- **notionr**

    To start the OAuth flow to verify to Notion that you would like the notionr integration to have access to your workspace:
    ```r
    notion_auth()
    #> Waiting for authentication in browser...
    #> Press Esc/Ctrl + C to abort
    #> Authentication complete.
    #> Access credentials have been stored at ~/.R/notionr/oauth/workspace-name/notionr_oauth_access.json
    ```
    Then, access the authorization token:
    ```r
    key <- cached_access_code()
    key
    #> [1] "secret_eolsOUT1yhjncXdT0wLBz74i3MGDXAe6P**********"
    ```
- **Internal Integration**

    Alternatively, if you want to create an internal integration, go to [your integrations](https://www.notion.so/my-integrations/)
    and create a new internal integration. From within the integration dashboard you can copy your authorization token and
    store it as desired.
    
- **Public Integration**

    notionr is not yet set up to accomodate a pubic integration.
