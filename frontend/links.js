export function pattern(language, patternName) {
    return `/pattern/${encodeURI(language)}/${encodeURI(patternName)}`
}

export function language(language) {
    return `/languages/${encodeURI(language)}`
}

export function search(searchFor) {
    return `/search/${encodeURI(searchFor)}`
}