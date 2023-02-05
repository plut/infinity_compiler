//! Full definition of all structures representing game resources.
//!
//! This crate makes heavy use of the `Pack` and `Table` derive macros to
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
	/// The resource type (as encoded in bif files).
	const RESTYPE: Restype;
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
		Self::for_each(db, format!(r#"where "id"='{resref}'"#),
		|resref, resource, subresources| {
			resource.show(resref, subresources);
			Ok(())
		}).unwrap();
		Ok(())
	}
} // trait ToplevelResource
/// When provided with a file extension, return the corresponding Restype.
pub fn restype_from_extension(ext: &str)->Restype {
	if ext.eq_ignore_ascii_case(Item::EXTENSION) { return Item::RESTYPE }
	Restype { value: 0 }
}

/// An effect inside a .itm file (either global or in an ability).
#[derive(Debug,Pack,Table)]
#[allow(missing_copy_implementations)]
#[resource(item_effects)] pub struct ItemEffect {
#[column(itemref, Resref, r#"references "items"("itemref")"#)]
#[column(abref, usize, r#"references "item_abilities"("abref")"#)]
#[column(effectref, usize)]
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
#[resource(item_abilities)] pub struct ItemAbility {
#[column(itemref, Resref, r#"references "items"("itemref")"#)]
#[column(abref, usize)]
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
#[resource(items)] pub struct Item {
#[header("ITM V1  ")]
#[column(itemref, Resref)]
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
	const RESTYPE: Restype = Restype { value: 0x03ed };
	type Subresources<'a> = (&'a mut [ItemAbility], &'a mut[(ItemEffect,usize)]);
	/// load an item from cursor
	fn load(db: &mut DbInserter<'_>, mut cursor: impl Read+Seek, resref: Resref) -> Result<()> {
		let item = Item::unpack(&mut cursor)?;
		item.ins(&mut db.tables.items, &(resref,))
			.context("inserting into 'items'")?;

		cursor.seek(SeekFrom::Start(item.abilities_offset as u64))?;
		// effect count per ability
		let mut ab_n = Vec::<u16>::with_capacity(item.abilities_count as usize);
		for abref in 0..item.abilities_count {
			let ab = ItemAbility::unpack(&mut cursor)?;
			ab.ins(&mut db.tables.item_abilities, &(resref, 1+abref as usize))
				.context("inserting into 'item_abilities'")?;
			ab_n.push(ab.effect_count);
		}
		trace!("inserting item {resref}; effects per abilities: {ab_n:?}");
		cursor.seek(SeekFrom::Start(item.effect_offset as u64))?;
		for j in 0..item.equip_effect_count { // on-equip effects
			let eff = ItemEffect::unpack(&mut cursor)?;
			eff.ins(&mut db.tables.item_effects, &(resref, 0, 1+j as usize))?;
		}
		for (i, n) in ab_n.iter().enumerate() {
			for j in 0..*n {
				let eff = ItemEffect::unpack(&mut cursor)?;
				eff.ins(&mut db.tables.item_effects, &(resref, 1+i, 1+j as usize))?;
			}
		}
	Ok(())
	}
	///
	fn save(&mut self, mut file: impl Write+Debug, (abilities, effects): Self::Subresources<'_>) ->Result<()> {
		let mut current_effect_idx = self.equip_effect_count;
		for a in abilities.iter_mut() {
			a.effect_index = current_effect_idx;
			current_effect_idx+= a.effect_count;
		}
		self.abilities_offset = 114;
		self.effect_offset = 114 + 56*(self.abilities_count as u32);
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
			let mut abilities = Vec::<ItemAbility>::new();
			let mut effects = Vec::<(ItemEffect, usize)>::new();
			for x in sel_item_ab.iter((&itemref,))? {
				if x.is_db_malformed() { continue }
				let (ab, _) = x?;
				abilities.push(ab);
				item.abilities_count+= 1;
			}
			for x in sel_item_eff.iter((&itemref,))? {
				if x.is_db_malformed() { continue }
				let (eff, (_parent, ab_id, _eff_id)) = x?;
				if ab_id > 0 {
					abilities[ab_id-1].effect_count+=1;
				} else {
					item.equip_effect_count+= 1;
				}
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
produce_resource_list!();

/// A structure holding insertion statements for all resource types.
///
/// This structure does the main work for initially filling the database.
/// It also contains a statement for storing original resrefs.
#[derive(Debug)]
pub struct DbInserter<'a> {
// 	db: &'a Connection,
	add_resref: Statement<'a>,
	tables: AllResources<Statement<'a>>,
	add_override: Statement<'a>,
	resource_count: AllResources<usize>,
}
impl<'a> DbInserter<'a> {
	/// Creates a new `DbInserter` from a database and the list of all
	/// resources.
	pub fn new(db: &'a Connection)->Result<Self> {
		Ok(Self { // db,
		tables: RESOURCES.map(|schema, _|
			db.prepare(&schema.insert_statement("or ignore", "res_"))
			.with_context(|| format!("insert statement for table '{table}'",
				table = schema.table_name))
		)?,
		resource_count: RESOURCES.map(|_,_| Ok::<usize,rusqlite::Error>(0))?,
		add_resref: db.prepare(
			r#"insert or ignore into "resref_orig" values (?)"#)?,
		add_override: db.prepare(
			r#"insert or ignore into "override" values (?,?)"#)?,
	}) }
	/// Adds a new [`Resref`] to the table of original resrefs.
	pub fn register(&mut self, resref: &Resref)->rusqlite::Result<usize> {
		self.add_resref.execute((resref,))
	}
	/// Loads a new top-level resource
	pub fn load<T: ToplevelResource>(&mut self, resref: Resref, mut handle: crate::gamefiles::ResHandle<'_>)->Result<()> {
		T::load(self, handle.open()?, resref)?;
		if handle.is_override() {
			self.add_override.execute((resref, T::EXTENSION))?;
		}
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
