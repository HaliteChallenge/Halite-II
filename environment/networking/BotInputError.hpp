//
// Created by David Li on 7/20/17.
//

#ifndef ENVIRONMENT_BOTINPUTERROR_HPP
#define ENVIRONMENT_BOTINPUTERROR_HPP

#include <exception>
#include <string>
#include <core/Entity.hpp>

class BotInputError : std::exception {
private:
    std::string message;
public:
    explicit BotInputError(
        hlt::PlayerId player,
        const std::string& input,
        const std::string& explanation,
        std::string::difference_type location);
    virtual auto what() const noexcept -> const char* override;
};

#endif //ENVIRONMENT_BOTINPUTERROR_HPP
