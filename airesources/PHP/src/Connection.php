<?php

class Connection
{
    /**
     * @var Logger
     */
    private $logger;

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

    public function sendMove(string $move): void
    {
        $this->send(" $move ");
    }

    public function send(string $message): void
    {
        $this->logger->log('Connection SEND: '.$message);
        fwrite(STDOUT, $message."\n");
    }
}
