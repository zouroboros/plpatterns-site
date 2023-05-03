export function pattern(language, patternName) {
    return `/pattern/${encodeURI(language)}/${encodeURI(patternName)}`
}

export function language(language) {
    return `/languages/${encodeURI(language)}`
}

export function search(searchFor) {
    if (searchFor === undefined) {
        return `/search`
    }
    return `/search/${encodeURI(searchFor)}`
}