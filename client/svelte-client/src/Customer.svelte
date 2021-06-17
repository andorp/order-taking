<script>
  import { createEventDispatcher } from 'svelte';
  import { getIdentifier } from './client.js';
  export let firstName    = '';
  export let lastName     = '';
  export let emailAddress = '';

  const dispatch = createEventDispatcher();

  async function customerChange() {
    const identifier = await getIdentifier();
    dispatch('customerChange', {
      identifier: identifier,
      firstName: firstName,
      lastName: lastName,
      emailAddress: emailAddress
    });
  }
</script>

<div on:change="{customerChange}">
  <p>Customer</p>
  <label for="firstName">First Name</label>
  <input id="firstName" type="text" bind:value="{firstName}"/>
  <label for="lastName">Last Name</label>
  <input id="lastName" type="text" bind:value="{lastName}"/>
  <label for="emailAddress">Email Address</label>
  <input id="emailAddress" type="email" bind:value="{emailAddress}">
</div>
