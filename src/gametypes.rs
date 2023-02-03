//! Full definition of all structures representing game resources.
//!
//! This crate makes heavy use of the `Pack` and `Table` derive macros to
//! automatically interface with game files and the SQL database.
//!
//! **Most** of the Lua-side definition of resource schemas is also
//! derived from here. The exception is the relation between resources
//! and their subresources. For now, this is inserted by hand by the
//! `init.lua` file.
use crate::prelude::*;
use macros::{produce_resource_list};
use crate::database::{Table,DbTypeCheck};
use crate::gamefiles::{Pack,Restype};

/// Those resources which are associated to a global table.
pub trait NamedTable: Table {
	/// Returns the associated field (e.g. `.item`) in an `AllResources` table.
	fn find_field<T: Debug>(all: &AllResources<T>)->&T;
	/// Same as above; `mut` case.
	fn find_field_mut<T: Debug>(all: &mut AllResources<T>)->&mut T;
}
/// A top-level resource (i.e. saved to its own file in the override
/// directory).
pub trait ToplevelResource: NamedTable {
	/// The file extension associated with this resource.
	const EXTENSION: &'static str;
	/// Subresources for this resource (together with linking info.).
	///
	/// This should be a tuple of all subresources, each of them stored in
	/// some `Vec` with elements (subresource, linking-info).
	type Subresources<'a>;
	/// Inserts a single resource in the database.
	fn load(db: &mut DbInserter<'_>, cursor: impl Read+Seek, resref: Resref)
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
		(db: &Connection, condition: impl Display, f:F)->Result<i32>;
	/// Saves all toplevel resources of this type.
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
	/// Shows a resource on stdout (grouped with its subresources) in a
	/// nice user format.
	fn show(&self, resref: impl Display, subresources: Self::Subresources<'_>);
	/// Shows on stdout the resource with given resref, or returns an error.
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

/// An effect inside a .itm file (either global or in an ability).
#[derive(Debug,Pack,Table)]
#[allow(missing_copy_implementations)]
#[resource(item_effects,itemref,items)] pub struct ItemEffect {
#[column(effectref, auto, "primary key")]
#[column(itemref, Resref, r#"references "items"("itemref")"#)]
#[column(abref, Option<i64>, r#"references "item_abilities"("abref")"#)]
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
#[derive(Debug,Pack,Table)]
#[allow(missing_copy_implementations)]
#[resource(item_abilities,itemref,items)] pub struct ItemAbility {
#[column(itemref, Resref, r#"references "items"("itemref")"#)]
#[column(abref, auto, "primary key")]
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
#[column(false)] effect_count: u16, // = 0,
#[column(false)] effect_index: u16, // = 0,
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
#[allow(missing_copy_implementations)]
#[resource(items,itemref,items)] pub struct Item {
#[header("ITM V1  ")]
#[column(itemref, Resref, "primary key")]
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
#[column(false)] abilities_offset: u32,
#[column(false)] abilities_count: u16,
#[column(false)] effect_offset: u32,
#[column(false)] effect_index: u16,
#[column(false)] equip_effect_count: u16,
}
impl ToplevelResource for Item {
	const EXTENSION: &'static str = "itm";
	type Subresources<'a> = (&'a mut [(ItemAbility,i64)], &'a mut[(ItemEffect,usize)]);
	/// load an item from cursor
	fn load(db: &mut DbInserter<'_>, mut cursor: impl Read+Seek, resref: Resref) -> Result<()> {
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
		(db: &Connection, condition: impl Display, f:F)->Result<i32> {
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
				let (eff, (_, _, parent)) = x?;
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
//  - the `AllResource<T>` type constructor,
//  - its implementation of `map()`,
//  - and the constant `RESOURCES`, which holds the parent resources.
produce_resource_list!();

/// A structure holding insertion statements for all resource types.
///
/// This structure does the main work for initially filling the database.
/// It also contains a statement for storing original resrefs.
#[derive(Debug)]
pub struct DbInserter<'a> {
	db: &'a Connection,
	add_resref: Statement<'a>,
	tables: AllResources<Statement<'a>>,
	resource_count: AllResources<usize>,
}
impl<'a> DbInserter<'a> {
	/// Creates a new `DbInserter` from a database and the list of all
	/// resources.
	pub fn new(db: &'a Connection)->Result<Self> {
		Ok(Self { db,
		tables: RESOURCES.map(|schema, _| db.prepare(&schema.insert_statement("res_")) )?,
		resource_count: RESOURCES.map(|_,_| Ok::<usize,rusqlite::Error>(0))?,
		add_resref: db.prepare(r#"insert or ignore into "resref_orig" values (?)"#)?
	}) }
	/// Adds a new [`Resref`] to the table of original resrefs.
	pub fn register(&mut self, resref: &Resref)->rusqlite::Result<usize> {
		self.add_resref.execute((resref,))
	}
	/// Loads a new top-level resource
	pub fn load<T: ToplevelResource>(&mut self, mut handle: crate::gamefiles::ResHandle<'_>)->Result<()> {
		T::load(self, handle.open()?, handle.resref)?;
		*(<T as NamedTable>::find_field_mut(&mut self.resource_count))+=1;
		Ok(())
	}
}
impl Drop for DbInserter<'_> {
	fn drop(&mut self) {
		self.resource_count.map(|schema, n| {
			if *n > 0 {
				info!(r#"loaded {n} entries in table "{}""#, schema.table_name);
			}
			Ok::<(),rusqlite::Error>(())
		}).unwrap();
	}
}

// The constants indicating the various resource types also have their
// place in this mod:
/// See iesdp
pub const RESTYPE_ITM: Restype = Restype { value: 0x03ed };
