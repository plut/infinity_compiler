//! Full definition of all structures representing game resources.
//!
//! This crate makes heavy use of the `Pack` and `SqlRow` derive macros to
//! automatically interface with game files and the SQL database.
#![feature(trace_macros)]
use crate::prelude::*;
use crate::pack::{Pack,PackAll};
use crate::gamefiles::{Restype};
use crate::resources::{ResourceIO};
use macros::{ResourceTree};

/// An effect inside a .itm file (either global or in an ability).
#[derive(Debug,Pack,SqlRow,ResourceTree)]
// #[subresource(item_effects,itemref,items)]
#[allow(missing_copy_implementations)]
// #[resource(item_effects)]
pub struct ItemEffect {
	opcode: u16, //opcode,
	target: u8, // EffectTarget,
	power: u8,
	parameter1: u32,
	parameter2: u32,
	timing_mode: u8, // TimingMode,
	dispel_mode: u8, // DispelMode,
	duration: u32,
	proba1: u8,
	proba2: u8,
	resource: Resref,
	dice_thrown: i32,
	dice_sides: i32,
	saving_throw_type: u32,
	saving_throw_bonus: i32,
	stacking_id: u32,
}
/// An ability inside a .itm file.
#[derive(Debug,Pack,SqlRow,ResourceTree)]
#[allow(missing_copy_implementations)]
// #[resource(item_abilities)]
pub struct ItemAbility {
	attack_type: u8, // AttackType,
	must_identify: u8,
	location: u8,
	alternative_dice_sides: u8,
	use_icon: Resref,
	target_type: u8, // TargetType,
	target_count: u8,
	range: u16,
	launcher_required: u8,
	alternative_dice_thrown: u8,
	speed_factor: u8,
	alternative_damage_bonus: u8,
	thac0_bonus: u16,
	dice_sides: u8,
	primary_type: u8,
	dice_thrown: u8,
	secondary_type: u8,
	damage_bonus: u16,
	damage_type: u16, // DamageType,
#[sql(false)]
	effect_count: u16,
#[sql(false)]
	effect_index: u16,
	max_charges: u16,
	depletion: u16,
	flags: u32,
	projectile_animation: u16,
	overhand_chance: u16,
	backhand_chance: u16,
	thrust_chance: u16,
	is_arrow: u16,
	is_bolt: u16,
	is_bullet: u16,
	effects: Vec::<ItemEffect>,
// 	itemref: NotPacked::<Resref>,
// 	index: NotPacked::<u16>,
// 	id: Rowid,
}
/// A game item, corresponding to a .itm file.
#[derive(Debug,Pack,SqlRow,ResourceTree)]
pub struct Item {
#[header("ITM V1  ")]
	unidentified_name: Strref,
	name: Strref,
	replacement: Resref,
	flags: u32, // ItemFlags,
	itemtype: u16, // ItemType,
	usability: u32, // UsabilityFlags,
	animation: u16, // StaticString<2>,
	min_level: u16,
	min_strength: u16,
	min_strengthbonus: u8,
	kit1: u8,
	min_intelligence: u8,
	kit2: u8,
	min_dexterity: u8,
	kit3: u8,
	min_wisdom: u8,
	kit4: u8,
	min_constitution: u8,
	proficiency: u8, // WProf,
	min_charisma: u16,
	price: u32,
	stack_amount: u16,
	inventory_icon: Resref,
	lore: u16,
	ground_icon: Resref,
	weight: i32,
	unidentified_description: Strref,
	description: Strref,
	description_icon: Resref,
	enchantment: i32,
#[sql(false)] abilities_offset: u32,
#[sql(false)] abilities_count: u16,
#[sql(false)] effect_offset: u32,
#[sql(false)] effect_index: u16,
#[sql(false)] equip_effect_count: u16,
	effects: Vec::<ItemEffect>,
	abilities: Vec::<ItemAbility>,
}

impl ResourceIO for Item {
	const EXTENSION: &'static str = "itm";
	const RESTYPE: Restype = Restype(0x03ed);
	/// load an item from cursor
	fn load(mut io: impl Read+Seek)->Result<Self> {
		let mut item = Item::unpack(&mut io)
			.context("unpack Item main struct")?;

		io.seek(SeekFrom::Start(item.abilities_offset as u64))?;
		item.abilities = ItemAbility::vecunpack(&mut io,
			item.abilities_count as usize)?;

		io.seek(SeekFrom::Start(item.effect_offset as u64))?;
		item.effects = ItemEffect::vecunpack(&mut io,
			item.equip_effect_count as usize)?;
		for ability in item.abilities.iter_mut() {
			ability.effects = ItemEffect::vecunpack(&mut io,
				ability.effect_count as usize)?;
		}
		Ok(item)
	}
	fn save(&mut self, mut io: impl Write+Seek+Debug)->Result<()> {
		trace!("saving item with {} abilities and {} effects",
			self.abilities.len(), self.effects.len());
		self.abilities_offset = 114;
		self.abilities_count = self.abilities.len() as u16;
		self.effect_offset = 114 + 56*(self.abilities_count as u32);
		self.equip_effect_count = self.effects.len() as u16;

		let mut current_effect_idx = self.equip_effect_count;
		for ab in self.abilities.iter_mut() {
			ab.effect_count = ab.effects.len() as u16;
			ab.effect_index = current_effect_idx;
			current_effect_idx+= ab.effect_count;
		}

		self.pack(&mut io)?;
		self.abilities.pack_all(&mut io)?;
		self.effects.pack_all(&mut io)?;
		for ab in self.abilities.iter() {
			ab.effects.pack_all(&mut io)?;
		}
		Ok(())
	}
}

/// Builds the full list of schemas for all resource tables.
///
/// The syntax is as follows:
/// `table_name: StructType = header`,
/// where `header` is either:
///  - `Top("extension", resource_type)`,
///  - `Sub(parent [, root])`
///
/// In these:
///  - `extension` is the file extension for this resource (e.g. `"itm"`),
///  - `resource_type` is the numeric identifier (e.g. 0x03ed),
///  - `parent` is the identifier for the parent table,
///  - `root` is the identifier for the highest resource table (strictly
///  put, this could be derived from the parent relations, but
///  `macro_rules!` is not too practical for walking in trees).
macro_rules! tables {
	($($tablename:ident: $ty:ty = $which:ident ($($arg:tt)*));*$(;)?) => {
		#[allow(non_snake_case)]
		impl Restype {
			pub fn from(e: &str)->Self {
				$(table_restype!(e,$which($($arg)*)));*;
				Self(0)
			}
		}
	}
}
macro_rules! table_restype {
	{$e:ident, Sub($($arg:tt)*)} => { };
	{$e:ident, Top($ext:literal, $restype: literal)} => {
		if $e.eq_ignore_ascii_case($ext) { return Self($restype) }
	}
}
tables! {
	items: Item = Top ("itm", 0x03ed);
	item_abilities: ItemAbility = Sub (items);
	item_ability_effects: ItemEffect = Sub(item_abilities, items);
	item_effects: ItemEffect = Sub (items);
}
// trace_macros!(true);
// trace_macros!(false);

#[derive(Debug,SqlRow,ResourceTree)]
pub struct Root {
	items: Vec::<Item>,
}
