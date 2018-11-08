#![feature(test)]

extern crate test;
extern crate rand;

use std::rc::Rc;

extern crate ptree;
use ptree::item::StringItem;

extern crate arrayvec;
use arrayvec::ArrayString;

const RADIX_MAX_LEN: usize = 16;

#[derive(Clone, Debug)]
pub struct Radish {
	radix: ArrayString<[u8; RADIX_MAX_LEN]>,
	rest: Vec<(char, Rc<Radish>)>,
	value: Option<i32>
}

fn starts_with(a: &str, b: &str) -> bool {
	b.len() <= a.len() && b.chars().zip(a.chars()).all(|(b_c, a_c)| b_c == a_c)
}

impl Radish {
	pub fn new() -> Radish {
		Radish {
			radix: ArrayString::new(),
			rest: Vec::new(),
			value: None,
		}
	}

	fn with_value(key: &str, value: i32) -> Radish {
		if key.len() > RADIX_MAX_LEN {
			let mut to_split = &key[..];
			let mut splitted_list = Vec::new();

			while to_split.len() > RADIX_MAX_LEN {
				let mut split_pos = RADIX_MAX_LEN;

				while !to_split.is_char_boundary(split_pos) {
					split_pos -= 1;
				}

				let splitted = to_split.split_at(split_pos);
				to_split = splitted.1;

				splitted_list.push(splitted.0);
			}

			splitted_list.push(to_split);

			let mut rad = Radish {
				radix: ArrayString::from(splitted_list.pop().unwrap()).unwrap(),
				rest: Vec::new(),
				value: Some(value),
			};

			for splitted in splitted_list.iter().rev() {
				rad = Radish {
					radix: ArrayString::from(splitted).unwrap(),
					rest: vec![(rad.radix.chars().next().unwrap(), Rc::new(rad))],
					value: None,
				}
			}

			rad

		} else {
			Radish {
				radix: ArrayString::from(key).unwrap(),
				rest: Vec::new(),
				value: Some(value),
			}
		}
	}

	pub fn add(&self, key: &str, value: i32) -> Result<Radish, String> {
		if self.radix.is_empty() && self.rest.is_empty() {
			return Ok(Radish::with_value(key, value));
		}

		if starts_with(key, &self.radix) { // La racine reste la même, on ajoute une branche
			if key.len() == self.radix.len() {
				Err("The key already exists.".to_string())
			} else {
				let r = key.get(self.radix.len()..).unwrap();
				let r_first = r.chars().next().unwrap();

				let mut new_rest = self.rest.clone();
				match self.rest.binary_search_by(|(first_letter, _)| first_letter.cmp(&r_first)) {
					Ok(found_pos) => new_rest[found_pos].1 = Rc::new(self.rest[found_pos].1.add(r, value)?),

					Err(insert_pos) => new_rest.insert(insert_pos, (r_first, Rc::new(Radish::with_value(r, value)))),
				}

				Ok(Radish {
					radix: self.radix.clone(),
					rest: new_rest,
					value: self.value.clone()
				})
			}
		} else { // La racine change, 
			let new_root_radix_length = self.radix.chars().zip(key.chars()).position(|(r, k)| r != k).unwrap();

			let new_root_radix = &key[..new_root_radix_length];
			let new_branch_radix = &key[new_root_radix_length..];
			let old_root_becoming_branch_radix = &self.radix[new_root_radix_length..];

			let new_branch = Radish::with_value(new_branch_radix, value);
			
			let old_root_becoming_branch = Radish {
				radix: ArrayString::from(old_root_becoming_branch_radix).unwrap(),
				rest: self.rest.clone(),
				value: self.value.clone(),
			};

			let old_root_becoming_branch_radix_first = old_root_becoming_branch_radix.chars().next().unwrap();
			let new_branch_radix_first = new_branch_radix.chars().next().unwrap();

			let new_rest = if old_root_becoming_branch_radix_first < new_branch_radix_first {
				vec![(old_root_becoming_branch_radix_first, Rc::new(old_root_becoming_branch)), (new_branch_radix_first, Rc::new(new_branch))]
			} else {
				vec![(new_branch_radix_first, Rc::new(new_branch)), (old_root_becoming_branch_radix_first, Rc::new(old_root_becoming_branch))]
			};
			
			Ok(Radish {
				radix: ArrayString::from(new_root_radix).unwrap(),
				rest: new_rest,
				value: None
			})
		}
	}

	pub fn get(&self, key: &str) -> Option<&i32> {
		if starts_with(key, &self.radix) {
			if key.len() == self.radix.len() {
				self.value.as_ref()
			} else {
				let r = key.get(self.radix.len()..).unwrap();
				let r_first = r.chars().next().unwrap();

				match self.rest.binary_search_by(|(first_letter, _)| first_letter.cmp(&r_first)) {
					Ok(found_pos) => self.rest[found_pos].1.get(r),
					Err(_) => None
				}
			}
		} else {
			None
		}
	}

	pub fn to_tree(&self) -> StringItem {
		StringItem {
			text: format!("{}({})", &self.radix, &self.value.map(|val| val.to_string()).unwrap_or(String::from(""))),
			children: self.rest.iter().map(|(_, b)| b.to_tree()).collect()
		}
	}

	pub fn iter(&self) -> RadishIterator {
		RadishIterator {
			root: &self,
			stack: Vec::new(),
			father_radix: String::new(),
		}
	}
}

#[derive(Clone, Debug)]
pub struct RadishIterator<'a> {
	root: &'a Radish,
	stack: Vec<(usize, std::slice::Iter<'a, (char, Rc<Radish>)>)>,
	father_radix: String,
}

impl<'a> Iterator for RadishIterator<'a> {
	type Item = (String, &'a i32);

	fn next(&mut self) -> Option<Self::Item> {
		if !self.stack.is_empty() {
			if let Some((_,  rad)) = self.stack.last_mut().unwrap().1.next() {
				let pushed = if !rad.rest.is_empty() {
					self.stack.push((rad.radix.len(), rad.rest.iter()));
					self.father_radix.push_str(&rad.radix);

					true
				} else {
					false
				};

				if let Some(ref value) = rad.value {
					let mut radix = self.father_radix.clone();
					if !pushed {
						radix.push_str(&rad.radix);
					}

					Some((radix, value))
				} else {
					self.next()
				}
			} else {
				let pop_radix_len = self.stack.last().unwrap().0;
				let father_radix_len = self.father_radix.len();
				self.father_radix.truncate(father_radix_len - pop_radix_len);
				self.stack.pop();

				if self.stack.is_empty() {
					None
				} else {
					self.next()
				}
			}
		} else {
			self.stack.push((self.root.radix.len(), self.root.rest.iter()));
			self.father_radix.push_str(&self.root.radix);

			if let Some(ref value) = self.root.value {
				Some((self.root.radix.to_string(), value))
			} else {
				self.next()
			}
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use rand;
	use rand::Rng;
	use test::Bencher;

    #[test]
    fn set_and_get() {
        let rad = Radish::new();
        let rad = rad.add("lol", -1).unwrap();
        assert_eq!(rad.get("lol"), Some(&-1));
        assert_eq!(rad.get("lo"), None);
        assert_eq!(rad.get("lolo"), None);
        assert_eq!(rad.get("lololo"), None);
        assert_eq!(rad.get("loto"), None);
        assert_eq!(rad.get("pomme"), None);

        let rad = rad.add("lolo", 2).unwrap();
        assert_eq!(rad.get("lol"), Some(&-1));
        assert_eq!(rad.get("lo"), None);
        assert_eq!(rad.get("lolo"), Some(&2));
        assert_eq!(rad.get("lololo"), None);
        assert_eq!(rad.get("loto"), None);
        assert_eq!(rad.get("pomme"), None);

        let rad = rad.add("lololo", 12345).unwrap();
        assert_eq!(rad.get("lol"), Some(&-1));
        assert_eq!(rad.get("lo"), None);
        assert_eq!(rad.get("lolo"), Some(&2));
        assert_eq!(rad.get("lololo"), Some(&12345));
        assert_eq!(rad.get("loto"), None);
        assert_eq!(rad.get("pomme"), None);

        let rad = rad.add("loto", 89).unwrap();
        assert_eq!(rad.get("lol"), Some(&-1));
        assert_eq!(rad.get("lo"), None);
        assert_eq!(rad.get("lolo"), Some(&2));
        assert_eq!(rad.get("lololo"), Some(&12345));
        assert_eq!(rad.get("loto"), Some(&89));
        assert_eq!(rad.get("pomme"), None);

        let rad = rad.add("pomme", 42).unwrap();
        assert_eq!(rad.get("lol"), Some(&-1));
        assert_eq!(rad.get("lo"), None);
        assert_eq!(rad.get("lolo"), Some(&2));
        assert_eq!(rad.get("lololo"), Some(&12345));
        assert_eq!(rad.get("loto"), Some(&89));
        assert_eq!(rad.get("pomme"), Some(&42));

        let rad = rad.add("lola", 43).unwrap();
        assert_eq!(rad.get("lol"), Some(&-1));
        assert_eq!(rad.get("lo"), None);
        assert_eq!(rad.get("lolo"), Some(&2));
        assert_eq!(rad.get("lololo"), Some(&12345));
        assert_eq!(rad.get("loto"), Some(&89));
        assert_eq!(rad.get("pomme"), Some(&42));
        assert_eq!(rad.get("lola"), Some(&43));

        let rad = rad.add("a1a2a3a4a5a6a7a8a9", 18).unwrap();
        assert_eq!(rad.get("lol"), Some(&-1));
        assert_eq!(rad.get("lo"), None);
        assert_eq!(rad.get("lolo"), Some(&2));
        assert_eq!(rad.get("lololo"), Some(&12345));
        assert_eq!(rad.get("loto"), Some(&89));
        assert_eq!(rad.get("pomme"), Some(&42));
        assert_eq!(rad.get("lola"), Some(&43));
        assert_eq!(rad.get("a1a2a3a4a5a6a7a8a9"), Some(&18));

        assert!(rad.add("lololo", 8).is_err());

        ptree::print_tree(&rad.to_tree()).unwrap();
    }

    #[test]
    fn immutability() {
    	let rad1 = Radish::new().add("a", 1).unwrap();
    	assert_eq!(rad1.get("a"), Some(&1));
    	assert_eq!(rad1.get("b"), None);

    	let rad2 = rad1.add("b", 2).unwrap();
    	assert_eq!(rad2.get("a"), Some(&1));
    	assert_eq!(rad2.get("b"), Some(&2));
    	assert_eq!(rad1.get("a"), Some(&1));
    	assert_eq!(rad1.get("b"), None);
    }

    #[test]
    fn iter() {
    	let mut rad = Radish::new().add("a", 1).unwrap();
    	rad = rad.add("b", 2).unwrap();
    	rad = rad.add("af", 3).unwrap();
    	rad = rad.add("ab", 4).unwrap();
    	rad = rad.add("aba", 5).unwrap();
    	rad = rad.add("bazerty", 6).unwrap();
    	rad = rad.add("c", 7).unwrap();

    	let mut iter = rad.iter();
    	assert_eq!(iter.next(), Some((String::from("a"), &1)));
		assert_eq!(iter.next(), Some((String::from("ab"), &4)));
		assert_eq!(iter.next(), Some((String::from("aba"), &5)));
		assert_eq!(iter.next(), Some((String::from("af"), &3)));
		assert_eq!(iter.next(), Some((String::from("b"), &2)));
		assert_eq!(iter.next(), Some((String::from("bazerty"), &6)));
    	assert_eq!(iter.next(), Some((String::from("c"), &7)));
    	assert_eq!(iter.next(), None);
    }

    #[bench]
    fn add_100_ints(b: &mut Bencher) {
    	let mut rng = rand::thread_rng();

    	b.iter(|| {
    		(0..100).fold(Radish::new(), |rad, value| {
    			let size = rng.gen_range(10, 20);
    			let key = rng.sample_iter(&rand::distributions::Alphanumeric).take(size).collect::<String>();
    			let new = rad.add(&key, value).unwrap();
    			assert_eq!(new.get(&key), Some(&value));
    			new
    		})
    	})
    }

    #[bench]
    fn add_1000_ints(b: &mut Bencher) {
    	let mut rng = rand::thread_rng();

    	b.iter(|| {
    		(0..1000).fold(Radish::new(), |rad, value| {
    			let size = rng.gen_range(10, 20);
    			let key = rng.sample_iter(&rand::distributions::Alphanumeric).take(size).collect::<String>();
    			let new = rad.add(&key, value).unwrap();
    			assert_eq!(new.get(&key), Some(&value));
    			new
    		})
    	})
    }

    #[bench]
    fn add_10000_ints(b: &mut Bencher) {
    	let mut rng = rand::thread_rng();

    	b.iter(|| {
    		(0..10000).fold(Radish::new(), |rad, value| {
    			let size = rng.gen_range(10, 20);
    			let key = rng.sample_iter(&rand::distributions::Alphanumeric).take(size).collect::<String>();
    			let new = rad.add(&key, value).unwrap();
    			assert_eq!(new.get(&key), Some(&value));
    			new
    		})
    	})
    }

    #[bench]
    fn add_100000_ints(b: &mut Bencher) {
    	let mut rng = rand::thread_rng();

    	b.iter(|| {
    		(0..100000).fold(Radish::new(), |rad, value| {
    			let size = rng.gen_range(10, 20);
    			let key = rng.sample_iter(&rand::distributions::Alphanumeric).take(size).collect::<String>();
    			let new = rad.add(&key, value).unwrap();
    			assert_eq!(new.get(&key), Some(&value));
    			new
    		})
    	})
    }

    #[bench]
    fn iter_100000_ints(b: &mut Bencher) {
    	let mut rng = rand::thread_rng();

    	let rad = (0..100000).fold(Radish::new(), |rad, value| {
    		let size = rng.gen_range(10, 20);
    		let key = rng.sample_iter(&rand::distributions::Alphanumeric).take(size).collect::<String>();
    		rad.add(&key, value).unwrap()
    	});

    	b.iter(|| {
    		let mut rad_iter = rad.iter();
    		while let Some((_key, &_value)) = rad_iter.next() {

    		}
    	})
    }
}
