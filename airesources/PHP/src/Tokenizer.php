<?php

class Tokenizer
{
    /**
     * @var string[]
     */
    private $tokens;

    /**
     * @var int
     */
    private $pointer = 0;

    public function __construct(string $input)
    {
        $this->tokens = explode(' ', $input);
    }

    public function nextInt(): int
    {
        return (int) $this->next();
    }

    public function nextFloat(): float
    {
        return (float) $this->next();
    }

    public function nextBool(): bool
    {
        return (bool) $this->next();
    }

    private function next(): string
    {
        return $this->tokens[$this->pointer++];
    }
}
