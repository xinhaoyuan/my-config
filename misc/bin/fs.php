<?php

// modify this to you access code
$access_code = getenv("FS_ACCESS_CODE");

session_cache_expire(180);
session_start();

$is_local = $_SERVER['REMOTE_ADDR'] == "127.0.0.1";
$_SESSION['auth'] = 'no';

if (isset($_REQUEST['ac'])) {
    if ($access_code != FALSE && $_REQUEST['ac'] == $access_code) {
        $_SESSION['auth'] = 'yes';
    }
}
else if ($is_local) {
    $_SESSION['auth'] = 'yes';
}

if ($access_code != FALSE) {
    if ($_SESSION['auth'] != 'yes') {
        error_log("rejecting request from " . $_SERVER['REMOTE_ADDR']);
        echo 'not authorized ' . $_SERVER['REMOTE_ADDR'];
        return TRUE;
    }
}

$path = str_replace("../", "", strtok($_SERVER["REQUEST_URI"],'?'));
if (substr($path, -1) == "/") $path = substr($path, 0, -1);

$fpath = realpath($_SERVER['DOCUMENT_ROOT'] . $path);
if ($fpath == realpath(__FILE__)) return TRUE;

if (is_dir($fpath)) {
    chdir($fpath);
    $g = glob("*");
    usort($g,function($a,$b) {
            if(is_dir($a) == is_dir($b))
                return strnatcasecmp($a,$b);
            else
                return is_dir($a) ? -1 : 1;
        });
    echo implode("<br>",array_map(
                     function($a) {
                         global $path;
                         return '<a href="'. $path . '/' . $a.'">'.$a.'</a>';
                     }, $g));
    return TRUE;
} else return FALSE;

?>
