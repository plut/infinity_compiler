//! Full definition of all structures representing game resources.
//!
//! This crate makes heavy use of the `Pack` and `Resource0` derive macros to
//! automatically interface with game files and the SQL database.
//!
//! **Most** of the Lua-side definition of resource schemas is also
//! derived from here. The exception is the relation between resources
//! and their subresources. For now, this is inserted by hand by the
//! `init.lua` file.
//!
//! The tables share the following common structure:
//! 1. Payload fields: these carry actual game information and
//!    and are defined in game resources. These always come as the first
//!    columns so that `select` statements can load resource fields from
//!    predictible column indices.
//! 2. Context fields: these identify in which game resource, and where,
//!    this information is located. The first context field is always
//!    the resref for the main resource.
//!    These fields also carry a `unique` constraint.
//! 3. For subresources: a `rowid` alias (`integer primary key`).
//!    (Top-level resources don't need this:
//!    they have the resref as their primary key).
//!
//! The `integer primary key` is needed even for subresources:
//! namely, we want these to have a persistent identifier
//! for joins with the edit tables.
#![feature(trace_macros)]
use crate::prelude::*;
use macros::{all_resources,Resource0};
use crate::pack::{Pack,NotPacked};
use crate::sql_rows::{SqlRow,SqlRow0,NoSql,Rowid};
use crate::database::{DbTypeCheck,DbInterface};
use crate::resources::{Resource0,ToplevelResourceData,ToplevelResource};

/// A game item, corresponding to a .itm file.
#[derive(Debug,Pack,SqlRow0,Resource0)]
#[topresource("items", "itm", 0x03ed)]
pub struct Item0 {
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
	abilities_offset: NoSql::<u32>,
	abilities_count: NoSql::<u16>,
	effect_offset: NoSql::<u32>,
	effect_index: NoSql::<u16>,
	equip_effect_count: NoSql::<u16>,
#[column("primary key")]
	itemref: NotPacked::<Resref>,
}
/// An ability inside a .itm file.
#[derive(Debug,Pack,SqlRow0,Resource0)]
#[subresource(item_abilities,itemref,items)]
#[allow(missing_copy_implementations)]
// #[resource(item_abilities)]
pub struct ItemAbility0 {
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
	effect_count: NoSql::<u16>, // = 0,
	effect_index: NoSql::<u16>, // = 0,
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
#[column(r#"references "items"("itemref") on delete cascade"#)]
	itemref: NotPacked::<Resref>,
	index: NotPacked::<u16>,
	id: Rowid,
}
/// An effect inside a .itm file (either global or in an ability).
#[derive(Debug,Pack,SqlRow0,Resource0)]
#[subresource(item_effects,itemref,items)]
#[allow(missing_copy_implementations)]
// #[resource(item_effects)]
pub struct ItemEffect0 {
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
	itemref: NotPacked::<Resref>,
#[column(r#"references "item_abilities"("id") on delete cascade"#)]
	ability: NotPacked::<i64>,
	index: NotPacked::<u16>,
	id: Rowid,
}
/// An effect inside a .itm file (either global or in an ability).
#[derive(Debug,Pack,SqlRow)]
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
#[derive(Debug,Pack,SqlRow)]
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
// 	itemref: NotPacked::<Resref>,
// 	index: NotPacked::<u16>,
// 	id: Rowid,
}
/// A game item, corresponding to a .itm file.
#[derive(Debug,Pack,SqlRow)]
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
	abilities: Vec::<ItemAbility>,
	global_effects: Vec::<ItemEffect>,
// #[column("primary key")]
// 	itemref: NotPacked::<Resref>,
}

impl ToplevelResource for Item0 {
	type Subresources<'a> = (&'a mut [ItemAbility0], &'a mut[(ItemEffect0,usize)]);
	/// load an item from cursor
	fn load(tables: &mut AllTables<Statement<'_>>, db: &impl DbInterface, mut cursor: impl Read+Seek, resref: Resref) -> Result<()> {
		let item = Item::unpack(&mut cursor)
			.context("cannot unpack Item main struct")?;
		let n = item.bind_execute1(&mut tables.items, &resref)
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

			ability.bind_execute2(&mut tables.item_abilities, &resref, &ab_index)
				.context("inserting into 'item_abilities'")?;
			ab_info.push((ability.effect_count, db.last_insert_rowid()));
		}
		trace!("inserting item {resref}; abilities have {ab_info:?} effects");
		cursor.seek(SeekFrom::Start(item.effect_offset as u64))?;
		for j in 1..1+item.equip_effect_count { // on-equip effects
			let effect = ItemEffect::unpack(&mut cursor)
				.with_context(|| format!("cannot unpack global item effect {}/{}",
					j+1, item.equip_effect_count))?;
			effect.bind_execute2(&mut tables.item_effects, &resref, &j)
				.context("inserting into 'item_effects'")?;
		}
		for (n, ab_id) in ab_info.iter() {
			for j in 1..(1+*n) {
				let effect = ItemEffect::unpack(&mut cursor)
					.with_context(|| format!("cannot unpack item effect {}/{} for ability {}", j+1, n, ab_id))?;
				effect.bind_execute2(&mut tables.item_ability_effects, &ab_id, &j)
					.context("inserting into 'item_ability_effects'")?;
			}
		}
	Ok(())
	}
	/// saves an item (with its sub-resources) to a file
	fn save(&mut self, mut file: impl Write+Seek+Debug, (abilities, effects): Self::Subresources<'_>) ->Result<()> {
		trace!("saving item with {} abilities and {} effects",
			abilities.len(), effects.len());
		let mut current_effect_idx = self.equip_effect_count.unwrap();
		for a in abilities.iter_mut() {
			a.effect_index = current_effect_idx.into();
			current_effect_idx+= a.effect_count.unwrap();
		}
		self.abilities_offset = 114.into();
		self.effect_offset = (114 + 56*(self.abilities_count.unwrap() as u32)).into();
		println!("\x1b[34mitem is: {self:?}\x1b[m");

		self.pack(&mut file)
			.with_context(|| format!("cannot pack item to {file:?}"))?;
		// pack abilities:
		for a in abilities.iter() {
			a.pack(&mut file)
			.with_context(|| format!("cannot pack item ability to {file:?}"))?;
		}
		// pack effects: first on-equip, then grouped by ability
		for (e, j) in effects.iter() {
			if *j == 0 {
				e.pack(&mut file)?;
			}
		}
		for i in 0..self.abilities_count.unwrap() {
			for (e, j) in effects.iter() {
				if *j == (i+1).into() {
					e.pack(&mut file)?;
				}
			}
		}
		Ok(())
	}
	/// Selects a number of items from the database.
	fn for_each<F,C>(db: &impl DbInterface, condition: C, f:F)->Result<i32>
		where F: Fn(Resref, &mut Self, Self::Subresources<'_>)->Result<()>,
			C: Display {
		let mut sel_item = Item0::select_typed(db, &condition)?;
		let mut sel_item_ab = ItemAbility0::select_typed(db, r#"where "key"=?"#)?;
		let mut sel_item_eff = ItemEffect0::select_typed(db, r#"where "key"=?"#)?;
		let mut n_items = 0;
		debug!("processing items under condition: {condition}");
		for x in sel_item.iter(())? {
			if x.is_db_malformed() { continue }
			let mut item = x?;
			debug!("reading item: {}", item.itemref);
			// we store item effects & abilities in two vectors:
			//  - abilities = (ability, abref)
			//  - effect = (effect, ability index)
			let mut abilities = Vec::<ItemAbility0>::new();
			let mut effects = Vec::<(ItemEffect0, usize)>::new();
			for x in sel_item_ab.iter((&item.itemref,))? {
				if x.is_db_malformed() { continue }
				abilities.push(x?);
				item.abilities_count+= 1;
			}
			for x in sel_item_eff.iter((&item.itemref,))? {
				if x.is_db_malformed() { continue }
				let eff = x?;
				let index = abilities.iter().position(|ab|
				// we might throw an error here — but [`Iterator::position`]
				// does not accept [`Result`] values.
					ab.id.unwrap().expect("null rowid in item abilities")
						== eff.ability.unwrap());
				let ab_id = match index {
					Some(i) => { abilities[i].effect_count+= 1; i+1 },
					None => { item.equip_effect_count+= 1; 0 },
				};
				effects.push((eff, ab_id));
			}
			f(item.itemref.unwrap(), &mut item, (&mut abilities, &mut effects))?;
			n_items+= 1;
		}
		debug!("processed {n_items} items");
		Ok(n_items)
	}
	fn show(&self, itemref: impl Display, (abilities, _effects): Self::Subresources<'_>) {
		println!("\
[{itemref}] {name}/{unidentified_name} ${price} ⚖{weight} ?{lore}
Requires: {st}/{ste} St, {dx} Dx, {co} Co, {wi} Wi, {in} In, {ch} Cha
Proficiency: {prof}
", name=self.name, unidentified_name = self.unidentified_name,
	price=self.price, weight=self.weight, lore=self.lore,
	st=self.min_strength, ste=self.min_strengthbonus,
	dx=self.min_dexterity, co=self.min_constitution,
	wi=self.min_wisdom, in=self.min_intelligence, ch=self.min_charisma,
	prof=self.proficiency);
		for ability in abilities {
			println!("
Attack type: {atype}",
			atype=ability.attack_type);
		}
	}
}

// This last invocation closes the list of resources above, generating:
//  - the `AllResource<T>` type constructor,
//  - its implementation of `map()`,
//  - and the constant `RESOURCES`, which holds the parent resources.
all_resources!();

macro_rules! list_tables {
	($($tablename:ident: $ty:ty $(,$more:ident)*);*$(;)?) => {
		#[derive(Debug)]
		pub struct AllTables<X: Debug> {
			$(pub $tablename: X,)*
		}
		impl<X: Debug> AllTables<X> {
			pub fn map<Y,E,F>(&self, f: F)->Result<AllTables<Y>,E>
			where Y: Debug, F: Fn(&X)->Result<Y,E> {
				Ok(AllTables::<Y> {
					$($tablename: f(&self.$tablename)?,)*
				})
			}
		}
		use crate::schemas::Schema;
		pub const SCHEMAS: AllTables<Schema> = AllTables {
			$($tablename: Schema {
				name: stringify!($tablename),
				parent: table_parent!($($more),*),
				fields: <$ty as crate::sql_rows::SqlRow>::FIELDS,
			}),*
		};
	}
}
macro_rules! table_parent {
	($parent:ident) => { table_parent!($parent, $parent) };
	($parent:ident,$root:ident) => {
		Some((stringify!($parent), stringify!($root)))
	};
	() => { None }
}
// trace_macros!(true);
list_tables! {
	items: Item;
	item_abilities: ItemAbility, items;
	item_ability_effects: ItemEffect, item_abilities, items;
	item_effects: ItemEffect, items;
}
// trace_macros!(false);
