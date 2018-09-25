#![feature(test)]

extern crate test;
use test::Bencher;

extern crate rand;

extern crate ptree;
use ptree::item::StringItem;

#[derive(Clone, Debug)]
struct Radish {
	radix: String,
	rest: Vec<Radish>,
	value: Option<i32>
}

impl Radish {
	pub fn new(key: &str, value: i32) -> Radish {
		Radish {
			radix: String::from(key),
			value: Some(value),
			rest: Vec::new()
		}
	}

	pub fn add(&self, key: &str, value: i32) -> Result<Radish, String> {
		if key.starts_with(&self.radix) { // La racine reste la même, on ajoute une branche
			if key.len() == self.radix.len() {
				Err("The key already exists.".to_string())
			} else {
				let r = key.get(self.radix.len()..).unwrap();

				let mut new_root = self.clone();

				match new_root.rest.binary_search_by(|rad| rad.radix.chars().next().cmp(&r.chars().next())) {
					Ok(found_pos) => {
						new_root.rest[found_pos] = new_root.rest[found_pos].add(r, value)?;

						Ok(new_root)
					},

					Err(insert_pos) => {
						new_root.rest.insert(insert_pos, Radish::new(r, value));

						Ok(new_root)
					}
				}
			}
		} else { // La racine change, 
			let pos = key.chars().zip(self.radix.chars()).position(|(k, r)| k != r).unwrap();

			let new_root_radix = String::from(&key[..pos]);

			let new_branch = Radish::new(&key[pos..], value);
			
			let mut old_root_becoming_branch = self.clone();
			old_root_becoming_branch.radix = String::from(&old_root_becoming_branch.radix[pos..]);
			
			Ok(Radish {
				radix: new_root_radix,
				rest: vec![old_root_becoming_branch, new_branch],
				value: None
			})
		}
	}

	pub fn get(&self, key: &str) -> Option<i32> {
		if key.starts_with(&self.radix) {
			if key.len() == self.radix.len() {
				self.value
			} else {
				let r = key.get(self.radix.len()..).unwrap();

				self.rest.iter().filter_map(|rad| rad.get(r)).next()
			}
		} else {
			None
		}
	}

	pub fn to_tree(&self) -> StringItem {
		StringItem {
			text: format!("{}({})", &self.radix, &self.value.map(|val| val.to_string()).unwrap_or(String::from(""))),
			children: self.rest.iter().map(|b| b.to_tree()).collect()
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use rand;
	use rand::Rng;

    #[test]
    fn it_works() {
        let rad = Radish::new("lol", -1);
        assert_eq!(rad.get("lol"), Some(-1));
        assert_eq!(rad.get("lo"), None);
        assert_eq!(rad.get("lolo"), None);
        assert_eq!(rad.get("lololo"), None);
        assert_eq!(rad.get("loto"), None);
        assert_eq!(rad.get("pomme"), None);

        let rad = rad.add("lolo", 2).unwrap();
        assert_eq!(rad.get("lol"), Some(-1));
        assert_eq!(rad.get("lo"), None);
        assert_eq!(rad.get("lolo"), Some(2));
        assert_eq!(rad.get("lololo"), None);
        assert_eq!(rad.get("loto"), None);
        assert_eq!(rad.get("pomme"), None);

        let rad = rad.add("lololo", 12345).unwrap();
        assert_eq!(rad.get("lol"), Some(-1));
        assert_eq!(rad.get("lo"), None);
        assert_eq!(rad.get("lolo"), Some(2));
        assert_eq!(rad.get("lololo"), Some(12345));
        assert_eq!(rad.get("loto"), None);
        assert_eq!(rad.get("pomme"), None);

        let rad = rad.add("loto", 89).unwrap();
        assert_eq!(rad.get("lol"), Some(-1));
        assert_eq!(rad.get("lo"), None);
        assert_eq!(rad.get("lolo"), Some(2));
        assert_eq!(rad.get("lololo"), Some(12345));
        assert_eq!(rad.get("loto"), Some(89));
        assert_eq!(rad.get("pomme"), None);

        let rad = rad.add("pomme", 42).unwrap();
        assert_eq!(rad.get("lol"), Some(-1));
        assert_eq!(rad.get("lo"), None);
        assert_eq!(rad.get("lolo"), Some(2));
        assert_eq!(rad.get("lololo"), Some(12345));
        assert_eq!(rad.get("loto"), Some(89));
        assert_eq!(rad.get("pomme"), Some(42));

        let mut rng = rand::thread_rng();
        let rad = (0..1000).fold(Radish::new("first", -1), |rad, value| rad.add(&rng.sample_iter(&rand::distributions::Alphanumeric).take(10).collect::<String>(), value).unwrap());

        ptree::print_tree(&rad.to_tree());
    }

    #[bench]
    fn take_1000_randoms(b: &mut Bencher) {
    	let mut rng = rand::thread_rng();

    	b.iter(|| {
    		(0..1000).fold(String::from(""), |_, _| rng.sample_iter(&rand::distributions::Alphanumeric).take(10).collect::<String>())
    	})
    }

    #[bench]
    fn add_1000_ints(b: &mut Bencher) {
    	let mut rng = rand::thread_rng();

    	b.iter(|| {
    		(0..1000).fold(Radish::new("first", -1), |rad, value| rad.add(&rng.sample_iter(&rand::distributions::Alphanumeric).take(10).collect::<String>(), value).unwrap())
    	})
    }
}
