<?php

class Logger
{
    /**
     * @var string
     */
    private $filename;

    /**
     * @var bool
     */
    private $enabled;

    public function __construct(bool $enabled)
    {
        $this->filename = __DIR__.'/../logs/'.date('Y_m_d_H_i_s').'_'.uniqid(null, true).'.log';
        $this->enabled = $enabled;
    }

    public function log(string $message)
    {
        if ($this->enabled) {
            file_put_contents($this->filename, $message."\n", FILE_APPEND);
        }
    }
}
