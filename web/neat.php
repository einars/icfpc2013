<style>
i {
    background: #3c3;
    font-style: normal;
}
table {
    border-collapse: collapse;
    float: left;
    margin-right: 20px;
}
td, th {
    font-size: 12px;
}
th {
font-weight: normal;
padding-right: 10px;
}
span {
    background: #c33;
}
</style>
<?php

$js = file_get_contents('problems/current.js');
$js = json_decode($js, true);

$problems = exploderate($js);

echo '<div style="font-family: source code pro, monaco, monospace">';
gfx('simple', $problems->simple);
gfx('fold', $problems->fold);
gfx('tfold', $problems->tfold);
echo '</div>';

function gfx($text, $probs) {
    echo '<table>';
    $runs = array();
    foreach($probs as $prob) {
        $title = $text . ':' . $prob['size'];
        if ( ! isset($runs[$title])) {
            $runs[$title] = '';
        }

        if ( ! isset($prob['solved'])) {
            $runs[$title] .= '.';
        } else {
            $runs[$title] .= $prob['solved'] ? '<i>S</i>' : '<span>X</span>';
        }
    }
    foreach($runs as $k=>$v) {
        printf('<tr><th>%20s</th><td>%s</td></tr>', $k, $v);
    }
    echo '</table>';
}

function exploderate($js)
{
    $problems = (object)array(
        'simple' => array(),
        'fold' => array(),
        'tfold' => array(),
        'bonus' => array(),
    );
    foreach($js as $p) {
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


