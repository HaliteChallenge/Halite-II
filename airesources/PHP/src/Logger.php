<?php

class Logger
{
    /**
     * @var string
     */
    private $filename;

    public function __construct()
    {
        $this->filename = __DIR__.'/../logs/'.date('Y_m_d_H_i_s').'_'.uniqid(null, true).'.log';
    }

    public function log(string $message): void
    {
        file_put_contents($this->filename, $message."\n", FILE_APPEND);
    }
}
