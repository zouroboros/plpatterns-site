const endpoint = import.meta.env.VITE_APILOCATION

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
    const data = await query('query languageAndPattern($language: String!) { pattern(language: $language) { name } }', { language })
    return data.pattern
}

export async function loadPatternByLanguageAndName(language, name) {
    const data = await query('query pattern($language: String!, $name: String!) { pattern(language: $language, name: $name) { name, code, description } }', 
        { language, name })
    const [ pattern ] = data.pattern
    return pattern
}

export async function loadSearchResults(searchFor) {
    const data = await query('query pattern($searchFor: String!) { search(searchFor: $searchFor) { pattern { name, language { name }}}}', 
        { searchFor })
    return data.search
}