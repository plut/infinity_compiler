//! Full definition of all structures representing game resources.
//!
//! This crate makes heavy use of the `Pack` and `Table` derive macros to
//! automatically interface with game files and the SQL database.
use rusqlite::{Connection,Statement};
use anyhow::{Context,Result};
use std::fmt::{Display,Debug};
use std::fs::{File};
use std::io::{Read,Write,Seek,SeekFrom};
use macros::{Pack, Table, produce_resource_list};
use crate::{Resref,Strref};
use crate::database::{Schema,Table,DbTypeCheck};
use crate::gameindex::{Pack};
use log::{trace,debug,info,warn,error};

/// A top-level resource (i.e. saved to its own file in the override
/// directory).
pub trait ToplevelResource: Table {
	/// The file extension associated with this resource.
	const EXTENSION: &'static str;
	/// Subresources for this resource (together with linking info.).
	///
	/// This should be a tuple of all subresources, each of them stored in
	/// some `Vec` with elements (subresource, linking-info).
	type Subresources<'a>;
	/// Inserts a single resource in the database.
	fn load(db: &mut crate::DbInserter, cursor: impl Read+Seek, resref: Resref)
		->Result<()>;
	/// Saves a single resource (with its subresources) to game files.
	fn save(&mut self, file: impl Write+Debug, subresources: Self::Subresources<'_>)
		->Result<()>;
	/// Applies a closure over all resources together with their subresources.
	///
	/// Iterates a closure over items in the database (with their
	/// associated properties) matching the given condition.
	///
	/// This function links items with their sub-resources
	/// (e.g. item abilities and effects)
	/// and calls the passed closure on the resulting structure.
	///
	/// It returns the number of rows matched.
	fn for_each<F: Fn(Resref, &mut Self, Self::Subresources<'_>)->Result<()>>
		(db: &Connection, condition: impl std::fmt::Display, f:F)->Result<i32>;
	fn save_all(db: &Connection)->Result<()> {
		let name = <Self as Table>::SCHEMA.table_name;
		let condition = format!(r#"where "key" in "dirty_{name}""#);
		let n = Self::for_each(db, condition,
			|resref, resource, subresources| {
				trace!("start compiling {name}: {resref}");
				let filename = format!("{resref}.{ext}", ext = Self::EXTENSION);
				let mut file = File::create(&filename)
					.with_context(|| format!("cannot open output file: {filename}"))?;
				resource.save(&mut file, subresources)
		})?;
		info!("compiled {n} entries from {name} to override");
		Ok(())
	}
	fn show(&self, resref: impl Display, subresources: Self::Subresources<'_>);
	fn show_all(db: &Connection, resref: impl Display)->Result<()> {
		Self::for_each(db, format!(r#"where "{key}"='{resref}'"#,
			key=<Self as Table>::SCHEMA.primary_key),
		|resref, resource, subresources| {
			resource.show(resref, subresources);
			Ok(())
		}).unwrap();
		Ok(())
	}
} // trait ToplevelResource
/// A game string as present in a .tlk file.
#[derive(Debug,Default,Clone,Pack,Table)]
pub struct GameString {
#[column(strref, usize, "primary key")]
	pub flags: u16,
	pub sound: Resref,
	pub volume: i32,
	pub pitch: i32,
#[column(false)] pub delta: i32,
#[column(false)] pub strlen: i32,
	pub string: String,
}
/// An effect inside a .itm file (either global or in an ability).
#[derive(Debug,Pack,Table)]
#[resource(item_effects,itemref,items)] pub struct ItemEffect {
#[column(itemref, Resref, r#"references "items"("itemref")"#)]
#[column(abref, Option<i64>, r#"references "item_abilities"("abref")"#)]
	pub opcode: u16, //opcode,
	pub target: u8, // EffectTarget,
	pub power: u8,
	pub parameter1: u32,
	pub parameter2: u32,
	pub timing_mode: u8, // TimingMode,
	pub dispel_mode: u8, // DispelMode,
	pub duration: u32,
	pub proba1: u8,
	pub proba2: u8,
	pub resource: Resref,
	pub dice_thrown: i32,
	pub dice_sides: i32,
	pub saving_throw_type: u32,
	pub saving_throw_bonus: i32,
	pub stacking_id: u32,
}
/// An ability inside a .itm file.
#[derive(Debug,Pack,Table)]
#[resource(item_abilities,itemref,items)] pub struct ItemAbility {
#[column(itemref, Resref, r#"references "items"("itemref")"#)]
#[column(abref, auto, "primary key")]
	pub attack_type: u8, // AttackType,
	pub must_identify: u8,
	pub location: u8,
	pub alternative_dice_sides: u8,
	pub use_icon: Resref,
	pub target_type: u8, // TargetType,
	pub target_count: u8,
	pub range: u16,
	pub launcher_required: u8,
	pub alternative_dice_thrown: u8,
	pub speed_factor: u8,
	pub alternative_damage_bonus: u8,
	pub thac0_bonus: u16,
	pub dice_sides: u8,
	pub primary_type: u8,
	pub dice_thrown: u8,
	pub secondary_type: u8,
	pub damage_bonus: u16,
	pub damage_type: u16, // DamageType,
#[column(false)] pub effect_count: u16, // = 0,
#[column(false)] pub effect_index: u16, // = 0,
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
}
/// An item effect, corresponding to a .itm file.
#[derive(Debug,Pack,Table)]
#[resource(items,itemref,items)] pub struct Item {
#[header("ITM V1  ")]
#[column(itemref, Resref, "primary key")]
	pub unidentified_name: Strref,
	pub name: Strref,
	pub replacement: Resref,
	pub flags: u32, // ItemFlags,
	pub itemtype: u16, // ItemType,
	pub usability: u32, // UsabilityFlags,
	pub animation: u16, // StaticString<2>,
	pub min_level: u16,
	pub min_strength: u16,
	pub min_strengthbonus: u8,
	pub kit1: u8,
	pub min_intelligence: u8,
	pub kit2: u8,
	pub min_dexterity: u8,
	pub kit3: u8,
	pub min_wisdom: u8,
	pub kit4: u8,
	pub min_constitution: u8,
	pub proficiency: u8, // WProf,
	pub min_charisma: u16,
	pub price: u32,
	pub stack_amount: u16,
	pub inventory_icon: Resref,
	pub lore: u16,
	pub ground_icon: Resref,
	pub weight: i32,
	pub unidentified_description: Strref,
	pub description: Strref,
	pub description_icon: Resref,
	pub enchantment: i32,
#[column(false)] pub abilities_offset: u32,
#[column(false)] pub abilities_count: u16,
#[column(false)] pub effect_offset: u32,
#[column(false)] pub effect_index: u16,
#[column(false)] pub equip_effect_count: u16,
}
impl ToplevelResource for Item {
	const EXTENSION: &'static str = "itm";
	type Subresources<'a> = (&'a mut [(ItemAbility,i64)], &'a mut[(ItemEffect,usize)]);
	/// load an item from cursor
	fn load(db: &mut crate::DbInserter, mut cursor: impl Read+Seek, resref: Resref) -> Result<()> {
		let item = Item::unpack(&mut cursor)?;
		item.ins(&mut db.tables.items, &(resref,))?;

		cursor.seek(SeekFrom::Start(item.abilities_offset as u64))?;
		// TODO zip those vectors
		let mut ab_n = Vec::<u16>::with_capacity(item.abilities_count as usize);
		let mut ab_i = Vec::<i64>::with_capacity(item.abilities_count as usize);
		for _ in 0..item.abilities_count {
			let ab = ItemAbility::unpack(&mut cursor)?;
			ab.ins(&mut db.tables.item_abilities, &(resref,))?;
			ab_n.push(ab.effect_count);
			ab_i.push(db.db.last_insert_rowid());
		}
	// 	println!("inserting item {resref}: {ab_n:?} {ab_i:?}");
		cursor.seek(SeekFrom::Start(item.effect_offset as u64))?;
		for _ in 0..item.equip_effect_count { // on-equip effects
			let eff = ItemEffect::unpack(&mut cursor)?;
			// TODO swap with Some above!!
			eff.ins(&mut db.tables.item_effects, &(resref, None))?;
		}
		for (i, n) in ab_n.iter().enumerate() {
			for _ in 0..*n {
				let eff = ItemEffect::unpack(&mut cursor)?;
				eff.ins(&mut db.tables.item_effects, &(resref, Some(ab_i[i])))?;
			}
		}
	Ok(())
	}
	///
	fn save(&mut self, mut file: impl Write+Debug, (abilities, effects): Self::Subresources<'_>) ->Result<()> {
		let mut current_effect_idx = self.equip_effect_count;
		for (a, _) in abilities.iter_mut() {
			a.effect_index = current_effect_idx;
			current_effect_idx+= a.effect_count;
		}
		self.abilities_offset = 114;
		self.effect_offset = 114 + 56*(self.abilities_count as u32);
		println!("\x1b[34mitem is: {self:?}\x1b[m");

		self.pack(&mut file)
			.with_context(|| format!("cannot pack item to {file:?}"))?;
		// pack abilities:
		for (a, _) in abilities.iter() {
			a.pack(&mut file)
			.with_context(|| format!("cannot pack item ability to {file:?}"))?;
		}
		// pack effects: first on-equip, then grouped by ability
		for (e, j) in effects.iter() {
			if *j == usize::MAX { e.pack(&mut file)?; }
		}
		for i in 0..self.abilities_count {
			for (e, j) in effects.iter() { if *j == i.into() { e.pack(&mut file)?; } }
		}
		Ok(())
	}
	fn for_each<F: Fn(Resref, &mut Self, Self::Subresources<'_>)->Result<()>>
		(db: &Connection, condition: impl std::fmt::Display, f:F)->Result<i32> {
		let mut sel_item = Item::select_query(db, &condition)?;
		let mut sel_item_ab = ItemAbility::select_query(db, r#"where "key"=?"#)?;
		let mut sel_item_eff = ItemEffect::select_query(db, r#"where "key"=?"#)?;
		let mut n_items = 0;
		debug!("processing items under condition: {condition}");
		for x in sel_item.iter(())? {
			if x.is_db_malformed() { continue }
			let (mut item, (itemref,)) = x?;
			debug!("reading item: {itemref}");
			// we store item effects & abilities in two vectors:
			//  - abilities = (ability, abref)
			//  - effect = (effect, ability index)
			let mut abilities = Vec::<(ItemAbility, i64)>::new();
			let mut effects = Vec::<(ItemEffect, usize)>::new();
			for x in sel_item_ab.iter((&itemref,))? {
				if x.is_db_malformed() { continue }
				let (ab, (_, abref)) = x?;
				abilities.push((ab, abref));
				item.abilities_count+= 1;
			}
			for x in sel_item_eff.iter((&itemref,))? {
				if x.is_db_malformed() { continue }
				let (eff, (_, parent)) = x?;
				let ab_id = match abilities.iter().position(|&(_, abref)| Some(abref) == parent) {
					Some(i) => { abilities[i].0.effect_count+= 1; i },
					None    => { item.equip_effect_count+= 1; usize::MAX},
				};
				effects.push((eff, ab_id));
			}
			f(itemref, &mut item, (&mut abilities, &mut effects))?;
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
		for (ability, _) in abilities {
			println!("
Attack type: {atype}",
			atype=ability.attack_type);
		}
	}
}

// This last invocation closes the list of resources above, generating:
//  - the `ByResource<T>` type constructor,
//  - its implementation of `iter()` and an iterator
//  - and the constant holding the parent resources.
produce_resource_list!();

// The constants indicating the various resource types also have their
// place in this mod:
use crate::gameindex::Restype;
pub const RESTYPE_ITM: Restype = Restype { value: 0x03ed };
