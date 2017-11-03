<?php

class Connection
{
    /**
     * @var Logger
     */
    private $logger;

    /**
     * @var string[]
     */
    private $stack = [];

    public function __construct(Logger $logger)
    {
        $this->logger = $logger;
    }

    public function read(): string
    {
        $input = fgets(STDIN);
        if ($input === false) {
            exit;
        }

        $data = rtrim($input, "\n");
        $data = trim($data);
        $this->logger->log('Connection READ: '.$data);

        return $data;
    }

    public function move(string $move): void
    {
        $this->stack[] = $move;
    }

    public function flush(): void
    {
        $this->send(implode(' ', $this->stack));
        $this->stack = [];
    }

    public function send(string $message): void
    {
        $this->logger->log('Connection SEND: '.$message);
        fwrite(STDOUT, $message."\n");
    }
}
