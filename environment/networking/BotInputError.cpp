//
// Created by David Li on 7/20/17.
//

#include "BotInputError.hpp"


BotInputError::BotInputError(
    hlt::PlayerId player,
    const std::string& input,
    const std::string& explanation,
    std::string::difference_type location)
    : player(player), input(input), explanation(explanation), location(location) {
    message += "ERROR: Bot #";
    message += std::to_string((int) player);
    message += ": ";
    message += explanation;
    message += " (at character ";
    message += std::to_string(location + 1);
    message += ".)\nInput received from bot:\n";
    message += input;
}

auto BotInputError::what() const noexcept -> const char* {
    return message.data();
}

