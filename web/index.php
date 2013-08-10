<?php

if ( ! file_exists('problems')) {
    mkdir('problems');
}

$key = "0471LR96Uo35Eet4lsIzkYr8bcVDtdWjbv9WyBsovpsH1H";

if ( ! file_exists('problems/current.js') || isset ($_POST['update'])) {

    $url = 'http://icfpc2013.cloudapp.net/myproblems?auth=' . $key;
    $js = fetch_external($url);
    if ( ! $js) {
        die("Cannot fetch from $url");
    }

    if (file_exists('problems/current.js')) {
        // maybe store in history?
        $new_hash = md5($js);
        $existing_hash = md5_file('problems/current.js');
        if ($new_hash != $existing_hash) {
            rename('problems/current.js', 'problems/current.' . date('Ymd.His') . '.js');
        }
    }

    file_put_contents('problems/current.js', $js);
    header('Location: ?');
    exit;

}

$js = file_get_contents('problems/current.js');
$js = json_decode($js, true);

$probs = exploderate($js);

$requested_json = isset($_REQUEST['json']) ? $_REQUEST['json'] : null;

if ($requested_json == 'simple.json' or $requested_json == 'op.json') {
    dump_json($probs->op);
    exit;
}
if ($requested_json == 'fold.json') {
    dump_json($probs->fold);
    exit;
}
if ($requested_json == 'tfold.json') {
    dump_json($probs->tfold);
    exit;
}

html_prologue();
echo '<div class="status">';
printf('<form method="post" action="?"><input type="hidden" name="update" value="yes"><button type="submit">Refresh from ICFP server (age: %d min)</button></form>',
    (time() - filectime('problems/current.js')) / 60 );
printf('Solved: %d of %d, failed: %d<br>Unsolved <a href="simple.json">simple: %d</a>, <a href="fold.json">fold: %d</a>, <a href="tfold.json">tfold: %d</a>'
    , sizeof($probs->solved)
    , $probs->n_total
    , sizeof($probs->failed)
    , sizeof($probs->op)
    , sizeof($probs->fold)
    , sizeof($probs->tfold)
);
echo '</div>';
echo '<h2 id="op">Simple problems</h2>';
print_problems($probs->op, 'p-op');
echo '<h2 id="fold">fold problems</h2>';
print_problems($probs->fold, 'p-fold');
echo '<h2 id="tfold">Tfold problems</h2>';
print_problems($probs->tfold, 'p-tfold');

html_epilogue();

function print_problems($ps, $class)
{
    if ( ! $ps) {
        echo '<p>No unsolved problems.';
    } else {
        printf('<table class="problems %s">', $class);
        foreach($ps as $p) {
            echo '<tr>';
            printf('<td>%s</td>', $p['id']);
            printf('<td>%s</td>', $p['size']);
            printf('<td>%s</td>', implode('; ', $p['operators']));
            echo '</tr>';
        }
        echo '</table>';
    }
}


function fetch_external($url)
{
    $url = str_replace(' ', '%20', $url);
    $url = str_replace("'", '%27', $url);
    $c = curl_init($url);
    curl_setopt($c, CURLOPT_RETURNTRANSFER, 1);
    curl_setopt($c, CURLOPT_HEADER, 0);
    curl_setopt($c, CURLOPT_TIMEOUT, 10);
    $contents = curl_exec($c);
    curl_close($c);
    return $contents;
}

function exploderate($js)
{
    $problems = (object)array(
        'solved' => array(),
        'failed' => array(),
        'op' => array(),
        'fold' => array(),
        'tfold' => array(),
        'n_total' => sizeof($js),
    );
    foreach($js as $p) {
        if (isset($p['solved'])) {
            if ($p['solved'] == 1) {
                $problems->solved[] = $p;
            } else {
                $problems->failed[] = $p;
            }
        } else {
            if (in_array('tfold', $p['operators'])) {
                $problems->tfold[] = $p;
            } else if (in_array('fold', $p['operators'])) {
                $problems->fold[] = $p;
            } else {
                $problems->op[] = $p;
            }
        }
    }

    usort($problems->op, 'problem_sort');
    usort($problems->fold, 'problem_sort');
    usort($problems->tfold, 'problem_sort');

    return $problems;
}

function problem_sort($a, $b) {
    return $a['size'] - $b['size'];
}

function html_prologue()
{
    echo <<<CSS
<html><head><title>ICFP2013</title>
<style>
* {
font-family: arial, sans-serif;
margin: 0; padding: 0;
}
a {
    color: #335;
    margin: 0 4px;
}
a.anchor {
    color: #335;
    text-decoration: none;
    border-bottom: 1px dotted #888;
}
div.status {
    background: #ccc;
    color: #111;
    padding: 24px 30px;
    line-height: 150%;
}
div.status form {
    float: right;
}
div.status button {
    padding: 4px 16px;
}
table.problems {
    border-collapse: collapse;
    margin-left: 20px;
    margin-bottom: 32px
}
table.problems td {
    padding: 2px 10px;
    font-size: 14px;
}
table.p-tfold td {
    border-bottom: 1px solid #9c9;
}
h2 {
    padding-left: 30px;
}
h2#tfold {
    background-color: #9c9;
    color: white;
}
table.p-fold td {
    border-bottom: 1px solid #99c;
}
h2#fold {
    background-color: #99c;
    color: white;
}
table.p-op td {
    border-bottom: 1px solid #c99;
}
h2#op {
    background-color: #c99;
    color: white;
}

table.
</style></head><body>
CSS;
}
function html_epilogue()
{
    echo '</body></html>';
}

function dump_json($probs)
{
    Header('Content-type: text/json');
    echo json_encode($probs);
    exit;
}
?>

