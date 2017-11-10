import Foundation

class TokenStack {
    let tokens: [String]
    var idx = 0
    
    init(_ tokens: [String]) {
        self.tokens = tokens
    }
    
    func pop() -> String {
        defer { idx += 1 }
        return tokens[idx]
    }
    
    func isEmpty() -> Bool {
        return idx == tokens.count
    }
}
