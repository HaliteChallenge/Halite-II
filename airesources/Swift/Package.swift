// swift-tools-version:4.0
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "Halite2Bot",
    products: [
        .library(
            name: "hlt",
            targets: ["hlt"]),
        .executable(
            name: "MyBot",
            targets: ["MyBot"])
    ],
    dependencies: [
    ],
    targets: [
        .target(
            name: "hlt",
            dependencies: []),
        .target(
            name: "MyBot",
            dependencies: ["hlt"])
    ]
)
