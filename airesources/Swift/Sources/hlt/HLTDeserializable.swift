import Foundation

protocol HLTDeserializable {
    static func deserialize(_ tokens: TokenStack) -> Self
}
