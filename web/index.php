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

if ($requested_json == 'simple.json') {
    dump_json($probs->simple);
}
if ($requested_json == 'fold.json') {
    dump_json($probs->fold);
}
if ($requested_json == 'tfold.json') {
    dump_json($probs->tfold);
}
if ($requested_json == 'tfold.json') {
    dump_json($probs->bonus);
}

html_prologue();
echo '<div class="status">';
printf('<div class="percentage">%.1f%%</div>', 
    sizeof($probs->solved) * 100 / $probs->n_total);
printf('<form method="post" action="?"><input type="hidden" name="update" value="yes"><button type="submit">Refresh from ICFP server (%s, %d min ago)</button></form>',
    date('H:i', filectime('problems/current.js')), (time() - filectime('problems/current.js')) / 60 );


printf('Solved: %d of %d, failed: %d'
    , sizeof($probs->solved)
    , $probs->n_total
    , sizeof($probs->failed)
);

if ($probs->in_progress) {
    echo ', in progress: ' . sizeof($probs->in_progress);
}


echo '<br>';


printf('Unsolved <a href="simple.json">simple: %d</a>, <a href="fold.json">fold: %d</a>, <a href="tfold.json">tfold: %d</a> <a href="bonus.json">bonus: %d</a>'
    , sizeof($probs->simple)
    , sizeof($probs->fold)
    , sizeof($probs->tfold)
    , sizeof($probs->bonus)
);
echo '</div>';
echo '<h2 id="simple">Simple problems</h2>';
print_problems($probs->simple, 'p-simple');
echo '<h2 id="fold">fold problems</h2>';
print_problems($probs->fold, 'p-fold');
echo '<h2 id="tfold">Tfold problems</h2>';
print_problems($probs->tfold, 'p-tfold');
echo '<h2 id="bonus">Bonus problems</h2>';
print_problems($probs->bonus, 'p-bonus');

html_epilogue();

function print_problems($ps, $class)
{
    if ( ! $ps) {
        echo '<p>No unsolved problems.';
    } else {
        printf('<table class="problems %s">', $class);
        $last_size = 0;
        foreach($ps as $p) {
            if ($last_size == 0) $last_size = $p['size'];
            if ($p['size'] != $last_size) {
                echo '<tr class="sep">';
                $last_size = $p['size'];
            } else {
                echo '<tr>';
            }
            printf('<td class="tt">%s</td>', $p['id']);
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
        'simple' => array(),
        'fold' => array(),
        'tfold' => array(),
        'bonus' => array(),
        'in_progress' => array(),
        'n_total' => sizeof($js),
    );
    foreach($js as $p) {
        if (isset($p['solved'])) {
            if ($p['solved'] == 1) {
                $problems->solved[] = $p;
            } else {
                if ( ! $p['timeLeft']) {
                    $problems->failed[] = $p;
                } else {
                    $problems->in_progress[] = $p;
                }
            }
        } else {
            if (in_array('tfold', $p['operators'])) {
                $problems->tfold[] = $p;
            } else if (in_array('fold', $p['operators'])) {
                $problems->fold[] = $p;
            } else if (in_array('bonus', $p['operators'])) {
                $problems->bonus[] = $p;
            } else {
                $problems->simple[] = $p;
            }
        }
    }

    usort($problems->simple, 'problem_sort');
    usort($problems->fold, 'problem_sort');
    usort($problems->tfold, 'problem_sort');
    usort($problems->bonus, 'problem_sort');

    return $problems;
}

function problem_sort($a, $b) {
    if ($a['size'] == $b['size']) {
        return strcmp($a['id'], $b['id']);
    } else {
        return $a['size'] - $b['size'];
    }
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
    border-top: 1px dotted #888;
}
div.status {
    background: #ccc;
    color: #111;
    padding: 24px 30px;
    line-height: 150%;
}
.percentage {
    position: absolute;
    left: 500px;
    font-size: 24px;
    font-weight: bold;
    background: #933;
    color: white;
    border-radius: 10px;
    padding: 10px;
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
    margin-bottom: 32px;
}
table.problems td {
    padding: 2px 10px;
    font-size: 14px;
}
table.p-tfold td {
    border-top: 1px solid #9c9;
}
h2 {
    padding-left: 30px;
}

h2#tfold {
    background-color: #9c9;
    color: white;
}



table.p-fold td {
    border-top: 1px solid #ddf;
}
table.p-fold tr.sep td {
    border-top-color: #99c;
}
h2#fold {
    background-color: #99c;
    color: white;
}

table.p-simple td {
    border-top: 1px solid #fdd;
}
table.p-simple tr.sep td {
    border-top-color: #c99;
}
h2#simple {
    background-color: #c99;
    color: white;
}

table.p-bonus td {
    border-top: 1px solid #cc9;
}

h2#bonus {
    background-color: #cc9;
    color: white;
}

td.tt {
    font-family: source code pro, monaco, courier new, monospace;
}

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

