const endpoint = `http://localhost:3000/patterns`

async function query(query, variables = {}) {
    const response = await fetch(endpoint, {
        method: `POST`,
        body: JSON.stringify({
            operation: null,
            query,
            variables
        })
    })

    const result = await response.json();

    if (`errors` in result) {
        throw new Error("Query resulted in an error", { graphQlErrors: result.errors })
    }

    return result.data;
}

export async function loadLanguages() {
    const data = await query("{ languages { name } }")
    return data.languages
}

export async function loadPatternByLanguage(language) {
    // todo improve endpoint on the server
    const data = await query('query languageAndPattern($language: String!) { pattern(language: $language) { name } }', { language })
    console.log(data)
    return data.pattern
}