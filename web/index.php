<?php

if ( ! file_exists('problems')) {
    mkdir('problems');
}

$key = "0471LR96Uo35Eet4lsIzkYr8bcVDtdWjbv9WyBsovpsH1H";

if ( ! file_exists('problems/current.js') || isset ($_REQUEST['update'])) {

    $url = 'http://icfpc2013.cloudapp.net/myproblems?auth=' . $key;
    $js = fetch_external($url);
    if ( ! $js or strlen($js) < 20) {
        header('Location: ?update=yes');
        exit;
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
if ($requested_json == 'bonus.json') {
    $size = $_REQUEST['size'];
    $out = $probs->bonus;
    if ($size) {
        if (strpos($size, 'r') !== false) {
            $size = str_replace('r', '', $size);
            $probs->bonus = array_reverse($probs->bonus);
        }
        $used_sizes = explode(',', $size);
        $out = array();
        $sizes = array();
        foreach($used_sizes as $s) $sizes[$s] = $s;
        foreach($probs->bonus as $p) {
            if (isset($sizes[ $p['size'] ]) ) {
                $out[] = $p;
            }
        }
    }
    dump_json($out);
}

html_prologue();
echo '<div class="status">';

$max_coolness = 0;
$our_coolness = 0;
$our_crappiness = 0;
foreach($probs->all as $prob) $max_coolness += $prob['size'];
foreach($probs->solved as $prob) $our_coolness += $prob['size'];
foreach($probs->failed as $prob) $our_crappiness += 1; //$prob['size'];

printf('<div class="percentage"><table><tr><th rowspan="2">%.1f<span>%%</span></th><td>
    coolness:<br>crappiness:</td><td style="text-align:right"><b>%.1f</b><span>%%</span><br><b>%.1f</b><span>%%</span>
    </td></tr></table>
    </div>',
    sizeof($probs->solved) * 100 / $probs->n_total,
    $our_coolness * 100 / $max_coolness,
    $our_crappiness * 100 / $probs->n_total
);
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

$s_left = mktime(2, 0, 0, 8, 12, 2013) - time ();
if ($s_left > 0) {
    $hrs_left = floor($s_left / 3600);
    $left = $s_left % 3600;
    $min_left = floor($left / 60);
    printf('<br>Time left: <b>%dh %02dm</b> (%d probs left: %ds/prob)<br>'
        , $hrs_left
        , $min_left
        , ((sizeof($probs->bonus) + sizeof($probs->simple) + sizeof($probs->fold) + sizeof($probs->tfold)))
        , ($s_left / (sizeof($probs->bonus) + sizeof($probs->simple) + sizeof($probs->fold) + sizeof($probs->tfold)))
    );
}


printf('Unsolved <a href="simple.json">simple: %d</a>, <a href="fold.json">fold: %d</a>, <a href="tfold.json">tfold: %d</a> <a href="bonus.json">bonus: %d</a>'
    , sizeof($probs->simple)
    , sizeof($probs->fold)
    , sizeof($probs->tfold)
    , sizeof($probs->bonus)
);
echo '</div>';
#echo '<h2 id="bonus">Bonus problems</h2>';
print_problems($probs->bonus, 'p-bonus');

# echo '<h2 id="tfold">Tfold problems</h2>';
# print_problems($probs->tfold, 'p-tfold');
# echo '<h2 id="simple">Simple problems</h2>';
# print_problems($probs->simple, 'p-simple');
# echo '<h2 id="fold">fold problems</h2>';
# print_problems($probs->fold, 'p-fold');

html_epilogue();

function print_problems($ps, $class)
{
    if ( ! $ps) {
        echo '<p style="padding:30px">All solved.</p>';
    } else {
        printf('<table class="problems %s">', $class);
        $last_size = 0;
        foreach($ps as $p) {
            if ($last_size == 0) $last_size = $p['size'];
            $class = '';
            if ($p['size'] != $last_size) {
                $class .= 'sep ';
                $last_size = $p['size'];
            }
            if (isset($p['timeLeft'])) {
                $class .= 'in-progress ';
            }
            if ($class) {
                echo '<tr class="' . trim($class) . '">';
            } else {
                echo '<tr>';
            }
            printf('<td class="tt">%s</td>', $p['id']);
            printf('<td>%s</td>', $p['size']);
            printf('<td>%s</td>', implode(' ', $p['operators']));
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
        'all' => array(),
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
        $problems->all[] = $p;
        if ($p['timeLeft'] and ! $p['solved']) {
            $problems->in_progress[] = $p;
            unset($p['solved']);
        }
        if (isset($p['solved'])) {
            if ($p['solved'] == 1) {
                $problems->solved[] = $p;
            } else {
                if ( ! $p['timeLeft']) {
                    $problems->failed[] = $p;
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
        return strcasecmp($a['id'], $b['id']);
    } else {
        return $a['size'] - $b['size'];
    }
}

function html_prologue()
{
    echo <<<CSS
<html><head>

    <meta charset="utf8">
<meta http-equiv="refresh" content="30">
<title>ICFP2013</title>
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
    background: #933;
    color: white;
    border-radius: 10px;
    padding: 10px;
}
.percentage th {
    font-size: 24px;
    color: white;
    padding: 0 10px;
}
.percentage td {
    color: white;
    padding: 0 5px;
}
.percentage p{
    font-size: 16px;
    text-align: right;
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
    border-top: 1px solid #dfd;
}
table.p-tfold .sep td {
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

table.p-bonus .sep td {
    border-top: 1px solid #cc9;
}
table.p-bonus td {
    border-top: 1px solid #ffd;
}

h2#bonus {
    background-color: #cc9;
    color: white;
}

td.tt {
    font-family: source code pro, monaco, courier new, monospace;
}
tr.in-progress td {
    background: #ff9;
}

</style>
    </head><body>
CSS;
}
function html_epilogue()
{
    echo '</body></html>';
}

function dump_json($probs)
{
    Header('Content-type: text/json');
    if (substr($probs[0]['id'], 0, 3) == '2TU') {
        array_shift($probs);
    }
    if (substr($probs[0]['id'], 0, 3) == '3z3') {
        array_shift($probs);
    }
    if (substr($probs[0]['id'], 0, 3) == '4Te') {
        array_shift($probs);
    }
    echo json_encode($probs);
    exit;
}
?>

