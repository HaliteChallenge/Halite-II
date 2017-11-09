<?php

class Connection
{
    /**
     * @var string[]
     */
    private $stack = [];

    public function read(): string
    {
        $input = fgets(STDIN);
        if ($input === false) {
            exit;
        }

        $data = rtrim($input, "\n");
        $data = trim($data);
        Logger::log('Connection READ: '.$data);

        return $data;
    }

    public function move(string $move)
    {
        $this->stack[] = $move;
    }

    public function flush()
    {
        $this->send(implode(' ', $this->stack));
        $this->stack = [];
    }

    public function send(string $message)
    {
        Logger::log('Connection SEND: '.$message);
        fwrite(STDOUT, $message."\n");
    }
}
