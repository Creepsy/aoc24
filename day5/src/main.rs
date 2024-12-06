use std::{collections::HashSet, error::Error};

use petgraph::{algo::toposort, dot::{Config, Dot}, graph::DiGraph};

type PageOrderRule = (u32, u32);
type Update = Vec<u32>;

fn main() {
    let file = std::env::args().nth(1).expect("No input file given.");
    let input = std::fs::read_to_string(file).unwrap();
    let (rules, updates) = parse(input).expect("Invalid input format");
    println!("Part 1: {:?}", part1(&updates, &rules));
    println!("Part 1: {:?}", part2(&updates, &rules));
}

fn parse(input: String) -> Result<(Vec<PageOrderRule>, Vec<Update>), Box<dyn Error>> {
    const INVALID_INPUT_ERR: &str = "Invalid input format";
    let (rules, updates) = input.split_once("\n\n").ok_or(INVALID_INPUT_ERR)?;
    let updates: Vec<Update> = updates
        .lines()
        .map(|update| update
            .split(',')
            .map(|p| p.parse().expect(INVALID_INPUT_ERR))
            .collect()
        )
        .collect();
    let rules = rules
        .lines()
        .map(|rule| {
                let (l, r) = rule.split_once('|').ok_or(INVALID_INPUT_ERR)?;
                Ok::<PageOrderRule, Box<dyn Error>>((l.parse::<u32>()?, r.parse::<u32>()?))
            }       
        ).collect::<Result<Vec<PageOrderRule>, Box<dyn Error>>>()?;
    Ok((rules, updates))
}

fn validate(update: &Update, rules: &Vec<PageOrderRule>) -> bool {
    let order = page_order(&filter_relevant_rules(update, rules)).unwrap();
    for i in 0..update.len() - 1 {
        if order[update[i] as usize] >= order[update[i + 1] as usize] {
            return false
        }
    }
    true
}

fn filter_relevant_rules(update: &Update, rules: &Vec<PageOrderRule>) -> Vec<PageOrderRule> {
    let pages: HashSet<u32> = HashSet::from_iter(update.clone());
    rules.iter().filter(|(a, b)| pages.contains(a) && pages.contains(b)).map(PageOrderRule::clone).collect()
}

fn page_order(rules: &Vec<PageOrderRule>) -> Result<Vec<u32>, Box<dyn Error>> {
    let page_graph = DiGraph::<u32, ()>::from_edges(rules);
    let sorted_pages: Vec<usize> = toposort(&page_graph, None)
        .map(|nodes| nodes.iter().map(|i | i.index()).collect())
        .map_err(|i| ("Cyclic Graph containing node ".to_string() + &i.node_id().index().to_string()))?;
    let mut order = vec![0; sorted_pages.len()];
    for i in 0..sorted_pages.len() {
        order[sorted_pages[i]] = i as u32;
    }
    Ok(order)
}

fn correct(mut to_correct: Update, rules: &Vec<PageOrderRule>) -> Update {
    let order = page_order(&filter_relevant_rules(&to_correct, rules)).unwrap();
    to_correct.sort_by_key(|x| order[*x as usize]);
    to_correct
}

fn mid<T: Copy>(arr: &Vec<T>) -> T {
    arr[arr.len() / 2]
}

fn part1(updates: &Vec<Update>, order: &Vec<PageOrderRule>) -> u32 {
    let valid_updates = updates.iter().filter(|u| validate(u, &order)).collect::<Vec<&Update>>();
    valid_updates.iter().map(|u| mid(u)).sum()
}

fn part2(updates: &Vec<Update>, order: &Vec<PageOrderRule>) -> u32 {
    let invalid_updates = updates.iter().filter(|u| !validate(u, &order)).collect::<Vec<&Update>>();
    let corrected_updates = invalid_updates.iter()
        .map(|u| correct((*u).clone(), &order))
        .collect::<Vec<Update>>();
    corrected_updates.iter().map(|u| mid(u)).sum()
}