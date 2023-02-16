//! Full definition of all structures representing game resources.
//!
//! This crate makes heavy use of the `Pack` and `SqlRow` derive macros to
//! automatically interface with game files and the SQL database.
#![feature(trace_macros)]
use crate::prelude::*;
use crate::pack::{Pack,PackAll};
use crate::gamefiles::{Restype};
use crate::sql_rows::{SqlRow};
use crate::database::{DbInterface};
use crate::resources::{TopResource0,SubResource,ResourceIO};
use macros::{Resource,top_resources};

/// An effect inside a .itm file (either global or in an ability).
#[derive(Debug,Pack,SqlRow,Resource)]
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
#[derive(Debug,Pack,SqlRow,Resource)]
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
#[derive(Debug,Pack,SqlRow,Resource)]
#[top(items)]
// #[topresource("items", "itm", 0x03ed)]
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
	fn save(&mut self, mut file: impl Write+Seek+Debug)->Result<()> {
		trace!("saving item with {} abilities and {} effects",
			self.abilities.len(), self.effects.len());
		self.pack(&mut file)?;
		// pack abilities:
		self.abilities.pack_all(&mut file)?;
		// pack global effects:
		self.effects.pack_all(&mut file)?;
		// pack per-ability effects:
		for ab in self.abilities.iter() {
			ab.effects.pack_all(&mut file)?;
		}
		Ok(())
	}
}
impl TopResource0 for Item {
	const EXTENSION: &'static str = "itm";
	const RESTYPE: Restype = Restype(0x03ed);
	type Subresources = (Vec<(ItemAbility,Vec<ItemEffect>)>, Vec<ItemEffect>);
	/// load an item from cursor
	fn load(tables: &mut AllTables<Statement<'_>>, db: &impl DbInterface, mut cursor: impl Read+Seek, resref: Resref) -> Result<()> {
		let item = Item::unpack(&mut cursor)
			.context("cannot unpack Item main struct")?;
		let n = item.bind_execute1(&mut tables.items, resref)
			.context("inserting into 'items'")?;
		if n == 0 {
			warn!("skipped inserting item {resref}");
			return Ok(())
		}
		cursor.seek(SeekFrom::Start(item.abilities_offset as u64))?;

		let mut ab_info = Vec::<(u16,i64)>::
			with_capacity(item.abilities_count as usize);
		for ab_index in 1..1+item.abilities_count {
			let ability = ItemAbility::unpack(&mut cursor)
				.with_context(|| format!("cannot unpack item ability {}/{}",
					ab_index, item.abilities_count))?;

			ability.bind_execute2(&mut tables.item_abilities, resref, ab_index)
				.context("inserting into 'item_abilities'")?;
			ab_info.push((ability.effect_count, db.last_insert_rowid()));
		}
		trace!("inserting item {resref}; abilities have {ab_info:?} effects");
		cursor.seek(SeekFrom::Start(item.effect_offset as u64))?;
		for j in 1..1+item.equip_effect_count { // on-equip effects
			let effect = ItemEffect::unpack(&mut cursor)
				.with_context(|| format!("cannot unpack global item effect {}/{}",
					j+1, item.equip_effect_count))?;
			effect.bind_execute2(&mut tables.item_effects, resref, j)
				.context("inserting into 'item_effects'")?;
		}
		for (n, ab_id) in ab_info.iter() {
			for j in 1..(1+*n) {
				let effect = ItemEffect::unpack(&mut cursor)
					.with_context(|| format!("cannot unpack item effect {}/{} for ability {}", j+1, n, ab_id))?;
				effect.bind_execute2(&mut tables.item_ability_effects, ab_id, j)
					.context("inserting into 'item_ability_effects'")?;
			}
		}
	Ok(())
	}
	fn save9(&mut self, mut file: impl Write+Seek+Debug, (abilities, effects): &Self::Subresources) ->Result<()> {
		trace!("saving item with {} abilities and {} effects",
			abilities.len(), effects.len());
		self.pack(&mut file)?;
		// pack abilities:
		abilities.iter().map(|(a,_)| a).pack_all(&mut file)?;
		// pack global effects:
		effects.pack_all(&mut file)?;
		// pack per-ability effects:
		for (_, effects) in abilities.iter() {
			effects.pack_all(&mut file)?;
		}
		Ok(())
	}
	fn select_subresources(&mut self, tables: &mut AllTables<Statement<'_>>, itemref: Resref)->Result<Self::Subresources> {
		debug!("reading item: {}", itemref);
		let mut abilities = Vec::<(ItemAbility,Vec<ItemEffect>)>::new();
		let item_effects = ItemEffect::collect_rows9(&mut tables.item_effects, (itemref,))?;
		self.equip_effect_count = item_effects.len() as u16;
		let mut current_effect_idx = self.equip_effect_count;
		for x in ItemAbility::iter_rows9(&mut tables.item_abilities, (itemref,))? {
			let (ab_id, mut ability) = x?;
			let ab_effects = ItemEffect::collect_rows9(&mut tables.item_ability_effects, (ab_id,))?;
			ability.effect_count = ab_effects.len() as u16;
			ability.effect_index = current_effect_idx;
			current_effect_idx+= ability.effect_count;
			abilities.push((ability, ab_effects));
		}
		self.abilities_count = abilities.len() as u16;
		self.abilities_offset = 114;
		self.effect_offset = 114 + 56*(self.abilities_count as u32);
		Ok((abilities, item_effects))
	}
}
impl SubResource for ItemAbility { }
impl SubResource for ItemEffect { }

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
		#[derive(Debug)]
		pub struct AllTables<X: Debug> {
			$(pub $tablename: X,)*
		}
		impl<X: Debug> AllTables<X> {
			pub fn map<'a,Y,E,F>(&'a self, f: F)->Result<AllTables<Y>,E>
			where Y: Debug+'a, F: Fn(&'a X)->Result<Y,E> {
				Ok(AllTables::<Y> { $($tablename: f(&self.$tablename)?,)* })
			}
			pub fn map_mut<Y,E,F>(&self, mut f: F)->Result<AllTables<Y>,E>
			where Y: Debug, F: FnMut(&X)->Result<Y,E> {
				Ok(AllTables::<Y> { $($tablename: f(&self.$tablename)?,)* })
			}
			pub fn by_name(&self, table_name: &str)->Result<&X> {
				match table_name {
					$(stringify!($tablename) => Ok(&self.$tablename)),*,
				_ => Err(Error::UnknownTable(table_name.to_owned()).into())
				}
			}
			pub fn by_name_mut(&mut self, table_name: &str)->Result<&mut X> {
				match table_name {
					$(stringify!($tablename) => Ok(&mut self.$tablename)),*,
				_ => Err(Error::UnknownTable(table_name.to_owned()).into())
				}
			}
		}
		use crate::schemas::Schema;
		#[allow(non_snake_case)]
		pub fn SCHEMAS()->AllTables<Schema> {
				AllTables {
				$($tablename: Schema {
					name: stringify!($tablename).to_owned(),
					table_type: table_type!($which($($arg)*)),
					fields: <$ty as crate::sql_rows::SqlRow>::FIELDS,
				}),*
			}
		}
		impl Restype {
			pub fn from(e: &str)->Self {
				$(table_restype!(e,$which($($arg)*)));*;
				Self(0)
			}
		}
	}
}
macro_rules! table_type {
	{Top($ext:literal, $rt:literal)} => {
		crate::schemas::TableType::Top { extension: $ext, }
	};
	{Sub($parent:ident)} => { table_type!(Sub($parent, $parent)) };
	{Sub($parent:ident,$root:ident)} => {
		crate::schemas::TableType::Sub {
			parent: stringify!($parent),
			root: stringify!($root),
		}
	};
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
top_resources!();
// trace_macros!(false);
