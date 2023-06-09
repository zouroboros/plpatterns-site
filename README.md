# plpatterns-site

Code for [plpatterns.org](https://plpatterns.org). The backend is implemented
, in Haskell, as a [GraphQL](https://graphql.org/) endpoint. The frontend is 
implemented using pure JavaScript. 

## Development

- `src` contains the code to interact with the database.
- `app` contains the code for the graph ql endpoint.
- `frontend` contains the code for the frontend.

The backend is built using stack. In order to run the backend create a `.env`
file based on the `.env.example` file and run the `stack run` command in the base of the repository.

The frontend is built using npm + vite. In the frontend folder create a 
`.env` file based on the `.env.example` file and run `npm run dev`.

## Database structure

root
- javascript
    - file-reading-nodejs
        - info: meta information
        - example.js: code example

## Plan

- lazy loading of common mark
- compare side
- code download
- link to code on discord
- show language and description on search page
- improve search (search code, break search words)
- write a proper readme for site and pattern repo
- add favicon