<script>
  import { createEventDispatcher } from 'svelte';
  import { getIdentifier } from './client.js';

  export let addressLine1 = "L1";
  export let addressLine2 = "";
  export let addressLine3 = "";
  export let addressLine4 = "";
  export let city = "";
  export let postCode = "";

  const dispatch = createEventDispatcher();

  async function addressChange() {
    const identifier = await getIdentifier();
    dispatch('addressChange', {
      identifier: identifier,
      addressLine1: addressLine1,
      addressLine2: addressLine2,
      addressLine3: addressLine3,
      addressLine4: addressLine4,
      city: city,
      zipCode: postCode
    });
  }
</script>

<div on:change="{addressChange}">
  <h2>Address</h2>
  <div>
    <label for="addressLine1">AddressLine 1</label>
    <input type="text" id="addressLine1" bind:value="{addressLine1}"/>
  </div>
  <div>
    <label for="addressLine2">AddressLine 2</label>
    <input type="text" id="addressLine2" bind:value="{addressLine2}"/>
  </div>
  <div>
    <label for="addressLine3">AddressLine 3</label>
    <input type="text" id="addressLine3" bind:value="{addressLine3}"/>
  </div>
  <div>
    <label for="addressLine4">AddressLine 4</label>
    <input type="text" id="addressLine4" bind:value="{addressLine4}"/>
  </div>
  <div>
    <label for="city">City</label>
    <input type="text" id="city" bind:value="{city}"/>
  </div>
  <div>
    <label for="postcode">Post code</label>
    <input type="text" id="postcode" bind:value="{postCode}"/>
  </div>
</div>
