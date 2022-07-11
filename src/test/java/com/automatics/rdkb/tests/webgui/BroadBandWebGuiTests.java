/**
* Copyright 2021 Comcast Cable Communications Management, LLC
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
 *
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*
* SPDX-License-Identifier: Apache-2.0
*/

package com.automatics.rdkb.tests.webgui;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.testng.annotations.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientInfo;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.rdkb.webui.constants.BroadBandWebGuiElements;
import com.automatics.rdkb.webui.constants.BroadBandWebGuiTestConstant;
import com.automatics.rdkb.webui.page.LanSideBasePage;
import com.automatics.rdkb.webui.page.LanSidePageNavigation;
import com.automatics.rdkb.webui.page.LanWebGuiLoginPage;
import com.automatics.rdkb.webui.utils.BroadBandWebUiUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.webui.page.BroadBandCommonPage;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;

/**
 * Test class for Validating software details from Broad band WebGUi
 * 
 * @author Gnanaprakasham S
 */

public class BroadBandWebGuiTests extends AutomaticsTestBase {

	/**
	 * Test To verify local UI no longer supports the ability to save/restore the
	 * config file from Admin page
	 * 
	 * <ol>
	 * <li>Step 1.Login to the Lan GUI page with correct password and username.</li>
	 * <li>step2.check if SAVE CURRENT CONFIGURATION button available in Gateway >
	 * At a Glance page.</li>
	 * <li>step3.check if Restore Saved Configuration button available in Gateway >
	 * At a Glance page</li>
	 * 
	 * </ol>
	 * 
	 * @author Deepa Bada
	 * @refactor Athira
	 */
	@Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
	@TestDetails(testUID = "TC-RDKB-WEBUI-CONFIG-5003")
	public void testToVerifyRestoreOrSaveConfigUnavailabilityInLan(Dut device) {

		// Test case id
		String testCaseId = "TC-RDKB-WEBUI-CONFIG-503";
		// Test step number
		String stepNumber = "s1";
		// String to store the error message
		String errorMessage = null;
		// String to store the test case status
		boolean status = false;
		String isCaptivePortal = null;
		WebDriver webDriver = null;
		try {

			LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
			LOGGER.info("PRE-CONDITION STEPS");
			LOGGER.info("PRE-CONDITION : DESCRIPTION 1: Disable captive portal ");
			LOGGER.info(
					"PRE-CONDITION 1: ACTION : Execute below WebPA set command - \"Device.DeviceInfo.X_RDKCENTRAL-COM_CaptivePortalEnable\",\"value\":false}]}'");
			LOGGER.info(
					"PRE-CONDITION 1: EXPECTED : WebPA request should return success message with the value of captive portal set.");

			isCaptivePortal = tapEnv.executeWebPaCommand(device,
					BroadBandWebPaConstants.WEBPA_PARAM_CAPTIVE_PORTAL_ENABLE);

			if (CommonMethods.isNotNull(isCaptivePortal)
					&& BroadBandTestConstants.FALSE.equalsIgnoreCase(isCaptivePortal)) {
				LOGGER.info("PRE-CONDITION 1: ACTUAL : Verified Captive portal enable status is FALSE");
			} else {

				boolean preCondition = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_CAPTIVE_PORTAL_ENABLE, WebPaDataTypes.BOOLEAN.getValue(),
						BroadBandTestConstants.FALSE);
				LOGGER.info("PRE-CONDITION 1: ACTUAL : " + (preCondition ? "Successfully disabled Captive portal "
						: "Failed to disable Captive portal through WebPA parameter "
								+ BroadBandWebPaConstants.WEBPA_PARAM_CAPTIVE_PORTAL_ENABLE));
				if (!preCondition) {
					throw new TestException("Failed to Disable captive Portal using WebPA request. "
							+ BroadBandWebPaConstants.WEBPA_PARAM_CAPTIVE_PORTAL_ENABLE);
				}
			}

			LOGGER.info(
					"PRE-CONDITION 2: DESCRIPTION : Verify whether any device is connected through wifi to the Broadband Gateway");
			LOGGER.info(
					"PRE-CONDITION 2: EXPECTED : Broadband Gateway should be connected to the client device through wifi");
			Dut clientDut = BroadBandConnectedClientUtils
					.get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);

			status = (null != clientDut);
			if (status) {
				LOGGER.info("GOING TO WAIT FOR 2 MINUTES AFTER CONNECTING THE WIFI CLIENT.");
				tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
				LOGGER.info(
						"PRE-CONDITION 2: ACTUAL :  : Device has been connected with 2.4/5 GHZ private WI-FI network");
			} else {
				LOGGER.error("PRE-CONDITION 2: ACTUAL : " + errorMessage);
				throw new TestException("Unable to obtain a 2.4 GHz or 5GHz  Working Wi-Fi connected client!");
			}

			LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

			LOGGER.info("#######################################################################################");
			LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBUI-CONFIG-5003");
			LOGGER.info("TEST DESCRIPTION:Remove ability to upload/download config file from the local UI");
			LOGGER.info("STEP 1 : DESCRIPTION:Login to the Lan GUI page with correct password and username.");
			LOGGER.info("STEP 2: DESCRIPTION :check for the SAVE CURRENT CONFIGURATION button from GUI.");
			LOGGER.info("STEP 3:check for the Restore Saved Configuration button from GUI");
			LOGGER.info("#######################################################################################");

			/**
			 * step 1 : Launch the Admin page: Device should launch the admin page
			 * successfully .
			 */

			LOGGER.info("#######################################################################################");
			LOGGER.info("STEP 1: DESCRIPTION :Launch Admin Login page and Login");
			LOGGER.info("STEP 1: ACTION : Login to admin page");
			LOGGER.info("STEP 1: EXPECTED: Login should be successfull and Redirected to At a Glance Page");
			LOGGER.info("#######################################################################################");

			errorMessage = "Unable to Login to LanGUI page using Admin credential";

			status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, clientDut);
			webDriver = LanSideBasePage.getDriver();
			LOGGER.info("STEP 1: ACTUAL :  " + (status ? "Successfully logged into admin page" : errorMessage));

			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNumber,
					status, errorMessage, true);

			/**
			 * Step 2 :check if "SAVE CURRENT CONFIGURATION" button available in "Gateway >
			 * At a Glance" page
			 */

			LOGGER.info("######################################################");
			LOGGER.info(
					"STEP 2 : DESCRIPTION:  check if SAVE CURRENT CONFIGURATION button available in Gateway > At a Glance page");
			LOGGER.info("STEP 2 : ACTION: check for the SAVE CURRENT CONFIGURATION button from GUI.");
			LOGGER.info("STEP 2 : EXPECTED: SAVE CURRENT CONFIGURATION button must not present");
			LOGGER.info("######################################################");
			stepNumber = "s2";
			status = false;
			errorMessage = "Unable to verify SAVE CURRENT CONFIGURATION button existence";
			status = BroadBandWebUiUtils.verifySaveOrRestoreConfigurationsButtonExists(
					BroadBandWebGuiElements.ELEMENT_XPATH_SAVE_CURRENT_CONFIGURATIONS, webDriver);

			LOGGER.info("STEP 2: ACTUAL :  "
					+ (status ? "Verified SAVE CURRENT CONFIGURATION button is not present" : errorMessage));

			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNumber,
					status, errorMessage, true);

			/**
			 * Step 3 :check if "Restore Saved Configuration" button available in "Gateway >
			 * At a Glance" page
			 */
			LOGGER.info("######################################################");
			LOGGER.info(
					"STEP 3 : DESCRIPTION:  check if Restore Saved Configuration button available in Gateway > At a Glance page");
			LOGGER.info("STEP 3 : ACTION: check for the Restore Saved Configuration button from GUI.");
			LOGGER.info("STEP 3 : EXPECTED: Restore Saved Configuration button must not present");
			LOGGER.info("######################################################");
			stepNumber = "s3";
			status = false;
			errorMessage = "unable to verify Restore Saved Configuration button avialability ";
			status = BroadBandWebUiUtils.verifySaveOrRestoreConfigurationsButtonExists(
					BroadBandWebGuiElements.ELEMENT_XPATH_RESTORE_SAVED_CONFIGURATIONS, webDriver);

			LOGGER.info("STEP 3: ACTUAL :  "
					+ (status ? "Verified Restore Saved Configuration button is not present" : errorMessage));

			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNumber,
					status, errorMessage, true);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(
					"EXCEPTION OCCURED WHILE VALIDATING RESTORE/SAVE CONFIG UNAVILABILITY IN LAN GUI" + errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
					false);
		} finally {
			LOGGER.info("POST-CONDITION : DESCRIPTION : Change captive portal value to default using WebPA");
			LOGGER.info(
					"POST-CONDITION : ACTION : Execute webpa set request for the parameter \"Device.DeviceInfo.X_RDKCENTRAL-COM_CaptivePortalEnable\"");
			LOGGER.info(
					"POST-CONDITION : EXPECTED : WebPA request should return success and default value should be set for Gateway IP");
			boolean postCondition = false;
			if (CommonMethods.isNotNull(isCaptivePortal)) {
				postCondition = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
						BroadBandWebPaConstants.WEBPA_PARAM_CAPTIVE_PORTAL_ENABLE, WebPaDataTypes.BOOLEAN.getValue(),
						isCaptivePortal);
			} else {
				LOGGER.info("Captive portal value obtained as null");
			}
			LOGGER.info("POST-CONDITION : ACTUAL : "
					+ (postCondition ? "Post condition executed successfully" : "Post condition failed"));
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WEBUI-CONFIG-5003");

	}

	/**
	 * Verify the details of connected clients can be retrieved from User admin page
	 * <ol>
	 * <li>Connect the client 1 to Private Wi-Fi Network and verify connection
	 * status</li>
	 * <li>Verify the IPv4 Address is retrieved from the client connected to Private
	 * Wi-Fi Network</li>
	 * <li>Verify the IPv6 Address is retrieved from the client connected to Private
	 * Wi-Fi Network</li>
	 * <li>Verify the internet is accessible in the client connected to Private
	 * Wi-Fi Network</li>
	 * <li>Verify the Wifi Mac address of wireless client is retrieved
	 * successfully</li>
	 * <li>Verify the Wired client connected to ethernet has been retrieved
	 * successfully</li>
	 * <li>Verify the client 2 connected to ethernet has IPv4 Address assigned from
	 * DHCP</li>
	 * <li>Verify the IPv6 Address is retrieved from the client connected to
	 * Ethernet</li>
	 * <li>Verify the internet is accessible in the client connected to
	 * Ethernet</li>
	 * <li>Verify the Mac address of wired client is retrieved successfully</li>
	 * <li>Launch Broad band User Admin page login page and verify login status</li>
	 * <li>Launch the Connected Devices page from Gateway > At a Glance page</li>
	 * <li>Verify the IP Address, Physical Address and Connection Type of all the
	 * online Devices listed can be retrieved from User admin page</li>
	 * <li>Cross-verify the IP Address, Physical MAC Address & Connection Type of
	 * the wired and wireless clients retrieved from User Admin page</li>
	 * </ol>
	 * 
	 * @param device {@link Instanceof Dut}
	 * @author SATHYA KISHORE
	 * @refactor Athira
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-CON-DEV-5001")
	public void testToVerifyConnectedClientDetailsInUserAdminPage(Dut device) {

		// Variable Declaration begins
		String testCaseId = "";
		String stepNum = "s1";
		String errorMessage = "";
		boolean status = false;
		Dut connectedClientDut = null;
		Dut wiredConnectedClientDut = null;
		List<String> ipAddressesRetrievedFromWirelessClient = null;
		List<String> ipAddressesRetrievedFromWiredClient = null;
		boolean isBrowserOpen = false;
		WebDriver webDriver = null;
		BroadBandConnectedClientInfo wirelessClient = new BroadBandConnectedClientInfo();
		BroadBandConnectedClientInfo wiredClient = new BroadBandConnectedClientInfo();
		List<BroadBandConnectedClientInfo> listOfActiveDevices = new ArrayList<BroadBandConnectedClientInfo>();
		List<BroadBandConnectedClientInfo> listOfActiveDevicesRetrievedFromWebGui = null;
		// Variable Declaration Ends

		testCaseId = "TC-RDKB-CON-DEV-501";

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-CON-DEV-5001");
		LOGGER.info("TEST DESCRIPTION: Verify the details of connected clients can be retrieved from User admin page");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Connect the client 1 to Private Wi-Fi Network and verify connection status ");
		LOGGER.info("2. Verify the IPv4 Address is retrieved  from the client connected to Private Wi-Fi Network");
		LOGGER.info("3. Verify the IPv6 Address is retrieved  from the client connected to Private Wi-Fi Network");
		LOGGER.info("4. Verify the internet is accessible in the client connected to Private Wi-Fi Network");
		LOGGER.info("5. Verify the Wifi Mac address of wireless client is retrieved successfully");
		LOGGER.info("6. Verify the Wired client connected to ethernet has been retrieved successfully");
		LOGGER.info("7. Verify the client 2 connected to ethernet has IPv4 Address assigned from DHCP");
		LOGGER.info("8. Verify the IPv6 Address is retrieved  from the client connected to Ethernet");
		LOGGER.info("9. Verify the internet is accessible in the client connected to Ethernet");
		LOGGER.info("10. Verify the Mac address of wired client is retrieved successfully");
		LOGGER.info("11. Launch Broad band User Admin page login page and verify login status");
		LOGGER.info("12. Launch the Connected Devices page from Gateway > At a Glance page");
		LOGGER.info(
				"13. Verify the IP Address, Physical Address and Connection Type of all the online Devices listed can be retrieved from User admin page");
		LOGGER.info(
				"14. Cross-verify the IP Address, Physical MAC Address & Connection Type of the wired and wireless clients retrieved from User Admin page");
		LOGGER.info("#######################################################################################");

		try {

			stepNum = "S1";
			errorMessage = "Unable to connect to the private Wi-Fi Network";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 1: DESCRIPTION : Connect the client 1 to Private Wi-Fi Network and verify connection status ");
			LOGGER.info(
					"STEP 1: ACTION : Connect to Private Wi-Fi using below commandsLinux :nmcli dev wifi connect <ssid> password <passwd>Windows : netsh wlan connect ssid=<ssid> name=<ssid name>");
			LOGGER.info("STEP 1: EXPECTED : Device should be connected with  Private Wi-Fi Network");
			LOGGER.info("**********************************************************************************");
			try {
				connectedClientDut = BroadBandConnectedClientUtils
						.get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
			} catch (Exception e) {
				errorMessage = e.getMessage();
				LOGGER.error(errorMessage);
			}
			status = null != connectedClientDut;
			boolean isWirelessClientAvailable = status;
			if (status) {
				wirelessClient.setConnectionType(BroadBandTestConstants.WIFI);
				LOGGER.info("GOING TO WAIT FOR 2 MINUTES AFTER CONNECTING THE WIFI CLIENT.");
				tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
				LOGGER.info("S1 ACTUAL: Client 1 has been connected with the Private Wi-Fi Network successfully");
			} else {
				LOGGER.error("S1 ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			if (isWirelessClientAvailable) {

				// step-2 to step-4
				ipAddressesRetrievedFromWirelessClient = BroadBandConnectedClientUtils
						.validateIpAddressesAndInternetConnectivityOfConnectedClient(device, connectedClientDut, tapEnv,
								BroadBandTestConstants.CONSTANT_2, testCaseId);
				for (String ipAddress : ipAddressesRetrievedFromWirelessClient) {
					if (CommonMethods.isIpv4Address(ipAddress)) {
						wirelessClient.setIpv4Address(ipAddress);
					}
				}

				stepNum = "S5";
				errorMessage = "Unable to retrieve the Physical Mac address of the client connected to Private Wi-Fi";
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP 5: DESCRIPTION : Verify the Wifi Mac address of wireless client is retrieved successfully");
				LOGGER.info("STEP 5: ACTION : Execute the command in the connected client :  ipconfig /all ");
				LOGGER.info("STEP 5: EXPECTED : Mac address of the Wi-Fi adapter should be retrieved successfully");
				LOGGER.info("**********************************************************************************");
				String macAddressOfWirelessClient = BroadBandConnectedClientUtils
						.getIpOrMacFromWindowsConnectedClient(connectedClientDut, tapEnv, false);
				LOGGER.info("Physical Mac Address of the client connected to Private Wi-Fi is : "
						+ macAddressOfWirelessClient);
				status = CommonMethods.isNotNull(macAddressOfWirelessClient)
						&& CommonMethods.isMacValid(macAddressOfWirelessClient);
				if (status) {
					macAddressOfWirelessClient = macAddressOfWirelessClient
							.replace(BroadBandTestConstants.COLON, BroadBandTestConstants.EMPTY_STRING).toUpperCase();
					wirelessClient.setMacAddress(macAddressOfWirelessClient);
					listOfActiveDevices.add(wirelessClient);
					LOGGER.info(
							"S5 ACTUAL : Successfully retrieved the Physical Mac address of the client connected to Private Wi-Fi");
				} else {
					LOGGER.error("S5 ACTUAL: " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			} else {
				LOGGER.error(
						"Unable to connect to Private Wi-Fi network, hence skipping S2 to S5 which invloves validation using Wi-Fi client.");
			}

			stepNum = "S6";
			errorMessage = "There is no client associated with the gateway through Ethernet";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 6: DESCRIPTION : Verify the Wired client connected to ethernet has been retrieved successfully");
			LOGGER.info(
					"STEP 6: ACTION : Get the ethernet client from the list of clients associated with the account");
			LOGGER.info("STEP 6: EXPECTED : Client connected to the Ethernet should be retrieved successfully");
			LOGGER.info("**********************************************************************************");
			wiredConnectedClientDut = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv, device);
			status = null != wiredConnectedClientDut;
			boolean isWiredClientAvailable = status;
			if (status) {
				wiredClient.setConnectionType(BroadBandTestConstants.CONNECTION_TYPE_ETHERNET);
				LOGGER.info("S6 ACTUAL: Client connected through ethernet has been retrieved successfully");
			} else {
				LOGGER.error("S6 ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			if (isWiredClientAvailable) {

				// step-7 to step-9
				ipAddressesRetrievedFromWiredClient = BroadBandConnectedClientUtils
						.validateIpAddressesAndInternetConnectivityOfConnectedClient(device, wiredConnectedClientDut,
								tapEnv, BroadBandTestConstants.CONSTANT_7, testCaseId);

				for (String ipAddress : ipAddressesRetrievedFromWiredClient) {
					if (CommonMethods.isIpv4Address(ipAddress)) {
						wiredClient.setIpv4Address(ipAddress);
					}
				}

				stepNum = "S10";
				errorMessage = "Unable to retrieve the Physical Mac address of the client connected to Ethernet";
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP 10: DESCRIPTION : Verify the Mac address of wired client is retrieved successfully");
				LOGGER.info("STEP 10: ACTION : Execute the command in the connected client :  ipconfig /all ");
				LOGGER.info("STEP 10: EXPECTED : Mac address of the Ethernet client should be retrieved successfully");
				LOGGER.info("**********************************************************************************");
				String macAddressOfWiredClient = BroadBandConnectedClientUtils
						.getIpOrMacFromLinuxConnectedClient(device, wiredConnectedClientDut, tapEnv, false);
				LOGGER.info("Physical Mac Address of the client connected to Ethernet is : " + macAddressOfWiredClient);
				status = CommonMethods.isNotNull(macAddressOfWiredClient)
						&& CommonMethods.isMacValid(macAddressOfWiredClient);
				if (status) {
					macAddressOfWiredClient = macAddressOfWiredClient
							.replace(BroadBandTestConstants.COLON, BroadBandTestConstants.EMPTY_STRING).toUpperCase();
					wiredClient.setMacAddress(macAddressOfWiredClient);
					listOfActiveDevices.add(wiredClient);
					LOGGER.info(
							"S10 ACTUAL : Successfully retrieved the Physical Mac address of the client connected to Ethernet");
				} else {
					LOGGER.error("S10 ACTUAL: " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			} else {
				LOGGER.error(
						"There is no client associated with the gateway through Ethernet , hence skipping S7 to S10 which invloves validation using Wi-Fi client.");
			}

			if (isWiredClientAvailable || isWirelessClientAvailable) {

				stepNum = "S11";
				errorMessage = "Unable to login gateway admin page";
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP 11: DESCRIPTION : Launch Broad band User Admin page login page and verify login status");
				LOGGER.info(
						"STEP 11: ACTION : Naviagte to AdminUI login page using admin/password credentials");
				LOGGER.info(
						"STEP 11: EXPECTED : Gateway > At a Glance page should be launched and login should be successful using admin credentials");
				LOGGER.info("**********************************************************************************");
				try {
					status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device,
							(isWirelessClientAvailable ? connectedClientDut : wiredConnectedClientDut));
					isBrowserOpen = status;
					webDriver = LanWebGuiLoginPage.getDriver();
				} catch (Exception exception) {
					LOGGER.error("Exception occurred during Gateway Admin Page login : " + exception.getMessage());
				}
				if (status) {
					LOGGER.info(
							"STEP 11: ACTUAL : Launch Broad band User Admin Page and verify login status is successful");
				} else {
					LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
						status, errorMessage, true);

				stepNum = "S12";
				errorMessage = "Unable to navigate Connected Devices page";
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info("STEP 12: DESCRIPTION : Launch the Connected Devices page from Gateway > At a Glance page");
				LOGGER.info("STEP 12: ACTION : Navigate to Devices  page by clicking on \"Connected Devices\" Menu ");
				LOGGER.info(
						"STEP 12: EXPECTED : Devices details page should get displayed having page title as \"Connected Devices >Devices\"");
				LOGGER.info("**********************************************************************************");
				try {

					status = LanSideBasePage.isPageLaunchedForPartners(device, tapEnv,
							BroadBandWebGuiTestConstant.LINK_TEXT_CONNECTED_DEVICES,
							BroadBandTestConstants.CONNECTED_DEVICES);
				} catch (Exception exception) {
					LOGGER.error("Exception occurred while navigating to connected devices page in LAN  GUI Page: "
							+ exception.getMessage());
				}
				if (status) {
					LOGGER.info(
							"STEP 12: ACTUAL : Connected Devices page has been launched successfully having Page Title as Gateway > Connected Devices");
				} else {
					LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
						status, errorMessage, true);

				stepNum = "S13";
				errorMessage = "Unable to retrieve details of active clients connected to gateway from user Admin Page";
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP 13: DESCRIPTION : Verify the IP Address, Physical Address and Connection Type of all the online Devices listed can be retrieved from User admin page");
				LOGGER.info(
						"STEP 13: ACTION : Retrieve the details of all the Online devices listed in  \"Connected Devices --> Online Device\" menu");
				LOGGER.info(
						"STEP 13: EXPECTED : Getting the list of Online device\"s  from WebUI should be successful ");
				LOGGER.info("**********************************************************************************");
				try {
					listOfActiveDevicesRetrievedFromWebGui = BroadBandCommonPage
							.getDetailsOfActiveDevicesFromGui(webDriver);
					if (listOfActiveDevicesRetrievedFromWebGui != null) {
						status = listOfActiveDevicesRetrievedFromWebGui.size() >= listOfActiveDevices.size();
						errorMessage = "No of online devices retrieved from WEB GUI do not match with the no of clients associated with the gateway";
					}
				} catch (Exception ex) {
					errorMessage = "Exception occurred while retrieving details of online clients from User Admin Page.";
					LOGGER.error(ex.getMessage());
				}
				if (status) {
					LOGGER.info(
							"STEP 13: ACTUAL : All the details of active clients connected to gateway have been retrieved successfully from user Admin page");
				} else {
					LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
						status, errorMessage, true);

				stepNum = "S14";
				errorMessage = "Connection Details retrieved from User Admin page and Client do not match";
				status = false;
				LOGGER.info("**********************************************************************************");
				LOGGER.info(
						"STEP 14: DESCRIPTION : Cross-verify the IP Address, Physical MAC Address & Connection Type of the wired and wireless clients retrieved from User Admin page");
				LOGGER.info(
						"STEP 14: ACTION : Compare the IP Address, Physical MAC Address & Connection Type retrieved from client with the value obtained from User Admin page");
				LOGGER.info(
						"STEP 14: EXPECTED : IP Address, Physical MAC Address & Connection Type retrived from user admin page should be same as the value obtained from client");
				LOGGER.info("**********************************************************************************");
				LOGGER.info("CONNECTION DETAILS FROM CLIENT : " + listOfActiveDevices.toString());
				LOGGER.info("CONNECTION DETAILS FROM WEBGUI: " + listOfActiveDevicesRetrievedFromWebGui.toString());
				int counter = BroadBandTestConstants.CONSTANT_0;
				for (BroadBandConnectedClientInfo clientDataRetrievedFromGui : listOfActiveDevicesRetrievedFromWebGui) {
					for (BroadBandConnectedClientInfo clientInfo : listOfActiveDevices) {
						if (BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
								clientDataRetrievedFromGui.getMacAddress(), clientInfo.getMacAddress())
								&& BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
										clientDataRetrievedFromGui.getIpv4Address(), clientInfo.getIpv4Address())
								&& CommonUtils.patternSearchFromTargetString(
										clientDataRetrievedFromGui.getConnectionType(),
										clientInfo.getConnectionType())) {
							counter++;
							LOGGER.info("COUNTER : " + counter);
						}
					}
				}
				status = (counter == listOfActiveDevices.size());
				if (status) {
					LOGGER.info(
							"STEP 14: ACTUAL : IP Address, Physical MAC Address & Connection Type retrived from user admin page is same as the value obtained from wired & wireless clients");
				} else {
					LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
				}
				LOGGER.info("**********************************************************************************");
				tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

			} else {
				LOGGER.error("Both Wi-Fi & Ethernet clients are not available , hence skipping the test !");
			}

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					true);
		} finally {
			if (isBrowserOpen) {
				try {
					LanSideBasePage.closeBrowser();
					LOGGER.info("Browser closed successfully");
				} catch (Exception exception) {
					LOGGER.error("Exception occurred while closing the browser, unable to close browser.");
				}
			}
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-CON-DEV-5001");
	}

	/**
	 * 
	 * @param device         instance of {@link Dut}
	 * @param wifiSSID       Wifi frequency band
	 * @param clientDevice   Connected Client Device
	 * @param wifiSsidName   SSID Name
	 * @param wifiPassPhrase Passphrase
	 * @param defaultChannel Default channel
	 */
	public void postConfigurationsForWifiRestoration(Dut device, WiFiFrequencyBand wifiSSID, Dut clientDevice,
			String wifiSsidName, String wifiPassPhrase, String defaultChannel) {
		boolean status = false;
		String errorMessage = null;
		try {
			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 1 : DESCRIPTION : Disconnect " + wifiSSID + " from the device");
			LOGGER.info("POST-CONDITION 1 : ACTION :Disconnect wifi radio " + wifiSSID);
			LOGGER.info("POST-CONDITION 1 : EXPECTED : Wifi " + wifiSSID + " should be disconnected successfully");
			LOGGER.info("#######################################################################################");
			LanWebGuiLoginPage.closeBrowser();
			errorMessage = "Unable to disconnect wifi " + wifiSSID;
			String ssidName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
					wifiSSID);
			LOGGER.info("SSIDNAME:" + ssidName);
			boolean connectionStatus = ConnectedNattedClientsUtils.disconnectSSID(clientDevice, tapEnv, ssidName);
			LOGGER.info("POST CONDITION 1:ACTUAL: WIFI " + wifiSSID + " Disconnect status:" + connectionStatus);
			if (connectionStatus) {
				LOGGER.info("POST CONDITION 1:ACTUAL: WIFI " + wifiSSID + " Disconnect status:" + connectionStatus);
			} else {
				LOGGER.error("POST CONDITION 1:ACTUAL: " + errorMessage);

			}
			status = false;
			errorMessage = "Unable to set " + wifiSSID + " SSID name to default";
			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 2 : DESCRIPTION :Restore " + wifiSSID + " name using Webpa");
			LOGGER.info("POST-CONDITION 2 : ACTION :Execute webpa command with Param " + wifiSSID
					+ " SSID with retrieved value");
			LOGGER.info("POST-CONDITION 2 : EXPECTED : Webpa set should be successful");
			LOGGER.info("#######################################################################################");
			errorMessage = "Unable to set " + wifiSSID + " SSID name to default";
			if (CommonMethods.isNotNull(wifiSsidName)) {
				switch (wifiSSID) {
				case WIFI_BAND_2_GHZ:
					status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME,
							BroadBandTestConstants.CONSTANT_0, wifiSsidName,
							BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
					break;
				case WIFI_BAND_5_GHZ:
					status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_NAME,
							BroadBandTestConstants.CONSTANT_0, wifiSsidName,
							BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
					break;
				default:
					break;
				}
			}
			if (status) {
				LOGGER.info("POST CONDITION 2:ACTUAL: WIFI SSID NAME OF 5GHZ Restore status:" + status);
			} else {
				LOGGER.error("POST CONDITION 2:ACTUAL: " + errorMessage);

			}
			status = false;
			errorMessage = "Unable to set " + wifiSSID + " SSID passphrase to default";
			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 3 : DESCRIPTION :Restore" + wifiSSID + " WiFi SSID PassPhrase using Webpa");
			LOGGER.info("POST-CONDITION 3 : ACTION :Execute webpa command with Param " + wifiSSID
					+ " passphrase with retrieved value ");
			LOGGER.info("POST-CONDITION 3 : EXPECTED : Webpa set should be successful");
			LOGGER.info("#######################################################################################");
			if (CommonMethods.isNotNull(wifiPassPhrase)) {
				switch (wifiSSID) {
				case WIFI_BAND_2_GHZ:
					status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2GHZ_SECURITY_KEYPASSPHRASE,
							BroadBandTestConstants.CONSTANT_0, wifiPassPhrase,
							BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
					break;
				case WIFI_BAND_5_GHZ:
					status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_5GHZ_SECURITY_KEYPASSPHRASE,
							BroadBandTestConstants.CONSTANT_0, wifiPassPhrase,
							BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
					break;
				default:
					break;
				}
			}
			if (status) {
				LOGGER.info(
						"POST CONDITION 3:ACTUAL: WIFI SSID PASSPHRASE OF " + wifiSSID + " Restore status:" + status);
			} else {
				LOGGER.error("POST CONDITION 3:ACTUAL: " + errorMessage);

			}
			status = false;
			errorMessage = "Unable to set " + wifiSSID + " SSID channel bandwidth to default";
			LOGGER.info("#######################################################################################");
			LOGGER.info("POST-CONDITION 4 : DESCRIPTION :Restore " + wifiSSID + " SSID Channel bandwidth using Webpa");
			LOGGER.info("POST-CONDITION 4 : ACTION :Execute webpa command with Param " + wifiSSID
					+ " channel bandwidth and retrieved value");
			LOGGER.info("POST-CONDITION 4 : EXPECTED : Webpa set should be successful");
			LOGGER.info("#######################################################################################");
			if (CommonMethods.isNotNull(defaultChannel)) {
				switch (wifiSSID) {
				case WIFI_BAND_2_GHZ:
					status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_FOR_OPERATING_BANDWIDTH_IN_2GHZ_BAND,
							BroadBandTestConstants.CONSTANT_0, defaultChannel,
							BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
					break;
				case WIFI_BAND_5_GHZ:
					status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
							BroadBandWebPaConstants.WEBPA_PARAM_FOR_OPERATING_BANDWIDTH_IN_5GHZ_BAND,
							BroadBandTestConstants.CONSTANT_0, defaultChannel,
							BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
							BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
					break;
				default:
					break;
				}
			}
			if (status) {
				LOGGER.info("POST CONDITION 4:ACTUAL: WIFI SSID Channel bandwidth OF " + wifiSSID + " Restore status:"
						+ status);
			} else {
				LOGGER.error("POST CONDITION 4:ACTUAL: " + errorMessage);

			}
		} catch (Exception e) {
			LOGGER.error("Exception caught in POST CONFIGURATIONS:" + e.getMessage());
			throw new TestException(BroadBandTestConstants.ERROR_LOG + errorMessage);
		}
	}

	/**
	 * Verify the Manage Sites and Managed Services should not accept rule which has
	 * Start time greater than End Time
	 * <ol>
	 * <li>Verify whether Private Wifi SSIDs\" are enabled using WebPA.</li>
	 * <li>Connect the client to Private Wi-Fi Network and verify connection
	 * status</li>
	 * <li>Verify the client connected to Private Wi-Fi Network has got the IPv4
	 * Address between DHCP Range</li>
	 * <li>Verify the client connected to Private Wi-Fi Network has got the IPv6
	 * Address</li>
	 * <li>Verify the internet is accessible in the client connected to the Private
	 * Wi-Fi Network</li>
	 * <li>Verify the gateway admin page on Wi-Fi client is launched and logged in
	 * successfully</li>
	 * <li>Launch the Managed Sites page from Gateway > At a Glance page</li>
	 * <li>Verify the Managed Sites feature can be enabled</li>
	 * <li>Launch the \"Managed Sites - Add Blocked Domain page</li>
	 * <li>Verify the Manage Sites - Add Blocked Domain should not accept rule which
	 * has Start time greater than End Time</li>
	 * <li>Launch the \"Managed Sites - Add Blocked Keyword page</li>
	 * <li>Verify the Manage Sites - Add Blocked Domain should not accept rule which
	 * has Start time greater than End Time</li>
	 * <li>Verify the Managed Sites feature can be disabled</li>
	 * <li>Launch the Managed Services page</li>
	 * <li>Verify the Managed Services feature can be enabled</li>
	 * <li>Launch the \"Managed Services -Add Blocked Service page</li>
	 * <li>Verify the Manage Services - Add Blocked Service should not accept rule
	 * which has Start time greater than End Time</li>
	 * <li>Verify the Managed Services feature can be disabled</li>
	 * </ol>
	 * 
	 * @author Gnanaprakasham S
	 * @refactor Rakesh C N
	 */
	@Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
	@TestDetails(testUID = "TC-RDKB-PARE-CTRL-NEG-5001")
	public void testToVerifyParentalControl(Dut device) {

		// Variable Declaration begins
		String testCaseId = "TC-RDKB-PARE-CTRL-NEG-501";
		String stepNum = "S1";
		String errorMessage = "";
		boolean status = false;
		// Variable Declation Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-PARE-CTRL-NEG-5001");
		LOGGER.info(
				"TEST DESCRIPTION: Verify the Manage Sites and Managed Services should not accept rule which has Start time greater than End Time");

		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify whether Private Wifi SSIDs\" are enabled using WebPA.");
		LOGGER.info("2. Connect the client to Private Wi-Fi Network and verify connection status ");
		LOGGER.info(
				"3. Verify the client connected to Private Wi-Fi Network has got the IPv4 Address between DHCP Range");
		LOGGER.info("4. Verify the client connected to Private Wi-Fi Network has got the IPv6 Address");
		LOGGER.info("5. Verify the internet is accessible in the client connected to the Private Wi-Fi Network");
		LOGGER.info("6. Verify the gateway admin page on Wi-Fi  client is launched and logged in successfully");
		LOGGER.info("7. Launch the Managed Sites page from Gateway > At a Glance page");
		LOGGER.info("8. Verify the Managed Sites feature can be enabled");
		LOGGER.info("9. Launch the \"Managed Sites - Add Blocked Domain page ");
		LOGGER.info(
				"10. Verify the Manage Sites - Add Blocked Domain should not accept rule which has Start time greater than End Time");
		LOGGER.info("11. Launch the \"Managed Sites - Add Blocked Keyword page ");
		LOGGER.info(
				"12. Verify the Manage Sites - Add Blocked Domain should not accept rule which has Start time greater than End Time");
		LOGGER.info("13. Verify the Managed Sites feature can be disabled");
		LOGGER.info("14. Launch the Managed Services page");
		LOGGER.info("15. Verify the Managed Services feature can be enabled");
		LOGGER.info("16. Launch the \"Managed Services -Add Blocked Service page ");
		LOGGER.info(
				"17. Verify the Manage Services - Add Blocked Service should not accept rule which has Start time greater than End Time");
		LOGGER.info("18. Verify the Managed Services feature can be disabled");

		LOGGER.info("#######################################################################################");

		try {

			stepNum = "s1";
			errorMessage = "Enabling Private Wi-Fi SSIDs' via WebPA failed";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Verify whether Private Wifi SSIDs\" are enabled using WebPA.");
			LOGGER.info(
					"STEP 1: ACTION : Execute the command to check Private Wifi SSIDs enabled or not");
			LOGGER.info("STEP 1: EXPECTED : Both 2.4 GHz and 5 GHz radio should be enabled");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Enabling 2.4 GHz private Wi-Fi radio via WebPA failed";
			if (BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
					WiFiFrequencyBand.WIFI_BAND_2_GHZ, tapEnv, device, true)) {
				status = BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
						WiFiFrequencyBand.WIFI_BAND_5_GHZ, tapEnv, device, true);
				errorMessage = "Enabling 5 GHz private Wi-Fi radio via WebPA failed";
			}
			if (status) {
				LOGGER.info("S1 ACTUAL: Both 2.4 GHz & 5 GHz Private Wi-Fi SSIDs' are enabled using WebPA");
			} else {
				LOGGER.error("S1 ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			stepNum = "s2";
			status = false;
			Dut wirelessConnectedClientSettop = null;
			errorMessage = "Unable to connect to the private Wi-Fi Network Or WiFi capable devices are not available";
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 2: DESCRIPTION : Connect the client to Private Wi-Fi Network and verify connection status ");
			LOGGER.info(
					"STEP 2: ACTION : Connect to the wifi using below commandsLinux :nmcli dev wifi connect <ssid> password <passwd>Windows : netsh wlan connect ssid=<ssid> name=<ssid name>");
			LOGGER.info("STEP 2: EXPECTED : Device should be connected with the Private Wi-Fi Network");
			LOGGER.info("**********************************************************************************");
			try {
				wirelessConnectedClientSettop = BroadBandConnectedClientUtils
						.get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
			} catch (Exception e) {
				errorMessage = e.getMessage();
				LOGGER.error(errorMessage);
			}
			status = null != wirelessConnectedClientSettop;
			if (status) {
				LOGGER.info("GOING TO WAIT FOR 2 MINUTES AFTER CONNECTING THE WIFI CLIENT.");
				tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
				LOGGER.info("S2 ACTUAL: Device has been connected with the Private Wi-Fi Network");
			} else {
				LOGGER.error("S2 ACTUAL: " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			// step-3 to step-5
			BroadBandConnectedClientUtils.validateIpAddressesAndInternetConnectivityOfConnectedClient(device,
					wirelessConnectedClientSettop, tapEnv, BroadBandTestConstants.CONSTANT_3, testCaseId);

			stepNum = "s6";
			status = false;
			errorMessage = "Unable to login gateway admin page";
			WebDriver webDriver = null;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 6: DESCRIPTION : Verify the gateway admin page on Wi-Fi  client is launched and logged in successfully");
			LOGGER.info("STEP 6: ACTION : Launch the URL in Wi-Fi client : http://10.0.0.1");
			LOGGER.info(
					"STEP 6: EXPECTED : Gateway admin page on Wi-Fi client should sucessfully get launched and logged in");
			LOGGER.info("**********************************************************************************");
			try {
				status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, wirelessConnectedClientSettop);
			} catch (Exception exception) {
				LOGGER.error("Exception occurred during Gateway Admin Page login : " + exception.getMessage());
			}
			if (status) {
				webDriver = LanSideBasePage.getDriver();
				LOGGER.info(
						"STEP 6: ACTUAL : Gateway admin page is accessible from the client connected using admin/****** credential for Residential or cusadmin/****** credential for Commercial devices");
			} else {
				LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNum = "s7";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 7: DESCRIPTION : Launch the Managed Sites page from Gateway > At a Glance page");
			LOGGER.info("STEP 7: ACTION : Navigate to  Parental Control > Managed Sites");
			LOGGER.info(
					"STEP 7: EXPECTED : Managed Sites page should be successfully launched with page title as \"Parental Control > Managed Sites\"");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to launch Managed Sites page from Gateway > At a Glance page";
			status = LanSideBasePage.isPageLaunchedByUsingWebElementforParentalControlManagedSites(device, tapEnv,
					BroadBandWebGuiTestConstant.LINK_TEXT_PARENTAL_CONTROL,
					BroadbandPropertyFileHandler.getParentalControlManagedSitesPageTitle());
			if (status) {
				LOGGER.info(
						"STEP 7: ACTUAL : Managed Sites page is successfully launched with page title as 'Parental Control > Managed Sites'");
			} else {
				LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNum = "s8";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 8: DESCRIPTION : Verify the Managed Sites feature can be enabled");
			LOGGER.info("STEP 8: ACTION : Click on \"Enable\" button in Managed sites page");
			LOGGER.info("STEP 8: EXPECTED : Managed Sites should be successfully enabled");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to enable Managed Sites feature!";
			status = BroadBandCommonPage.enableOrDisableRadioButton(tapEnv,
					BroadBandWebGuiElements.ELEMENT_XPATH_PARENTAL_CONTROL_MANAGED_SITES_ENABLE);
			if (status) {
				LOGGER.info("STEP 8: ACTUAL : Parental Control - Managed Sites is successfully enabled!");
			} else {
				LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNum = "s9";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 9: DESCRIPTION : Launch the \"Managed Sites - Add Blocked Domain page ");
			LOGGER.info("STEP 9: ACTION : Navigate to  Parental Control > Managed Sites > Add Blocked Domain");
			LOGGER.info(
					"STEP 9: EXPECTED : Managed Sites - Add Blocked Domain page should be successfully launched with page title as 'Parental Control > Managed Sites > Add Blocked Domain'");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to launch Managed Sites - Add Blocked Domain from Managed Sites page";
			status = LanSideBasePage.isPageLaunchedByUsingWebElementforParentalControlAddBlockDomain(device, tapEnv,
					By.xpath(BroadBandWebGuiElements.ELEMENT_XPATH_PARENTAL_CONTROL_MANAGED_SITES_ADD_SITES),
					BroadBandTestConstants.MANAGED_SITES_ADD_SITES);
			if (status) {
				LOGGER.info(
						"STEP 9: ACTUAL : Managed Sites - Add Blocked Domain page is successfully launched with page title as 'Parental Control > Managed Sites > Add Blocked Domain'");
			} else {
				LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNum = "s10";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 10: DESCRIPTION : Verify the Manage Sites - Add Blocked Domain should not accept rule which has Start time greater than End Time");
			LOGGER.info(
					"STEP 10: ACTION : Enter URL, Select 'Block Always' as 'No' and select 'Start From' greater than 'End On' time and click Save Button");
			LOGGER.info(
					"STEP 10: EXPECTED : Rule should not be save instead error should be thrown as 'Start time should be smaller than End time !' ");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to validate 'Manage Sites - Add Blocked Domain' rule with Start time greater than End Time";
			status = BroadBandCommonPage.addParentalControlInvalidRule(tapEnv,
					BroadBandTestConstants.MANAGED_SITES_ADD_SITES);
			if (status) {
				LOGGER.info(
						"STEP 10: ACTUAL : Rule is not saved, instead error is successfully thrown as 'Start time should be smaller than End time !'");
			} else {
				LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNum = "s11";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 11: DESCRIPTION : Launch the 'Managed Sites - Add Blocked Keyword' page ");
			LOGGER.info("STEP 11: ACTION : Navigate to  Parental Control > Managed Sites > Add Blocked Keyword");
			LOGGER.info(
					"STEP 11: EXPECTED : Managed Sites - Add Blocked Keyword page should be successfully launched with page title as 'Parental Control > Managed Sites > Add Blocked Keyword'");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to launch Managed Sites - Add Blocked Keyword from Managed Sites page";
			status = LanSideBasePage.isPageLaunchedByUsingWebElementforParentalControlAddBlockedKeyword(device, tapEnv,
					By.xpath(BroadBandWebGuiElements.ELEMENT_XPATH_PARENTAL_CONTROL_MANAGED_SITES_ADD_KEY_WORD),
					BroadBandTestConstants.MANAGED_SITES_ADD_KEYWORD);
			if (status) {
				LOGGER.info(
						"STEP 11: ACTUAL : Managed Sites - Add Blocked Keyword page is successfully launched with page title as 'Parental Control > Managed Sites > Add Blocked Keyword'");
			} else {
				LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNum = "s12";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 12: DESCRIPTION : Verify the 'Manage Sites - Add Blocked Keyword' should not accept rule which has Start time greater than End Time");
			LOGGER.info(
					"STEP 12: ACTION : Enter Keyword,  Select 'Block Always' as 'No' and select 'Start From' greater than 'End On' time and click Save Button");
			LOGGER.info(
					"STEP 12: EXPECTED : Rule should not be save instead error should be thrown as 'Start time should be smaller than End time!' ");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to validate 'Manage Sites - Add Blocked Keyword' rule with Start time greater than End Time";
			status = BroadBandCommonPage.addParentalControlInvalidRule(tapEnv,
					BroadBandTestConstants.MANAGED_SITES_ADD_KEYWORD);
			if (status) {
				LOGGER.info(
						"STEP 12: ACTUAL : Rule is not saved, instead error is successfully thrown as 'Start time should be smaller than End time !'");
			} else {
				LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNum = "s13";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 13: DESCRIPTION : Verify the Managed Sites feature can be disabled");
			LOGGER.info("STEP 13: ACTION : Click on \"Disable\" button in Managed sites page");
			LOGGER.info("STEP 13: EXPECTED :  Managed Sites should be successfully enabled");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to disable Managed Sites feature!";
			status = BroadBandCommonPage.enableOrDisableRadioButton(tapEnv,
					BroadBandWebGuiElements.ELEMENT_XPATH_PARENTAL_CONTROL_MANAGED_SITES_DISABLE);
			if (status) {
				LOGGER.info("STEP 13: ACTUAL : Parental Control - Managed Sites is successfully disabled!");
			} else {
				LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNum = "s14";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 14: DESCRIPTION : Launch the Managed Services page");
			LOGGER.info("STEP 14: ACTION : Navigate to Parental Control > Managed Services");
			LOGGER.info(
					"STEP 14: EXPECTED : Managed Services page should be successfully launched with page title as 'Parental Control > Managed Services '");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to launch Managed Services page from Managed Sites page";
			status = LanSideBasePage.isPageLaunchedByUsingWebElementforParentalControlManagedService(device, tapEnv,
					BroadBandWebGuiTestConstant.LINK_TEXT_MANAGED_SERVICES, BroadBandTestConstants.MANAGED_SERVICES);
			if (status) {
				LOGGER.info(
						"STEP 14: ACTUAL : Managed Services page is successfully launched with page title as 'Parental Control > Managed Services'");
			} else {
				LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNum = "s15";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 15: DESCRIPTION : Verify the Managed Services feature can be enabled");
			LOGGER.info("STEP 15: ACTION : Click on 'Enable' button in Managed Service page");
			LOGGER.info("STEP 15: EXPECTED :  Managed Services should be successfully enabled");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to enable Managed Services feature!";
			status = BroadBandCommonPage.enableOrDisableRadioButton(tapEnv,
					BroadBandWebGuiElements.ELEMENT_XPATH_PARENTAL_CONTROL_MANAGED_SERVICES_ENABLE);
			if (status) {
				LOGGER.info("STEP 15: ACTUAL : Parental Control - Managed Services is successfully enabled!");
			} else {
				LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNum = "s16";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 16: DESCRIPTION : Launch the 'Managed Services - Add Blocked Service' page ");
			LOGGER.info("STEP 16: ACTION : Navigate to  Parental Control > Managed Services > Add Blocked Service");
			LOGGER.info(
					"STEP 16: EXPECTED : Managed Services - Add Blocked Service page should be successfully launched with page title as 'Parental Control > Managed Services > Add Blocked Service'");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to launch Managed Services - Add Blocked Service from Managed Services page";
			status = LanSideBasePage.isPageLaunchedByUsingWebElementAddBlockedService(device, tapEnv,
					By.xpath(BroadBandWebGuiElements.ELEMENT_XPATH_PARENTAL_CONTROL_MANAGED_SERVICE_ADD_SERVICES),
					BroadBandTestConstants.MANAGED_SERVICES_ADD_SERVICE);
			if (status) {
				LOGGER.info(
						"STEP 16: ACTUAL : Managed Sites - Add Blocked Service page is successfully launched with page title as 'Parental Control > Managed Sites > Add Blocked Keyword'");
			} else {
				LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNum = "s17";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info(
					"STEP 17: DESCRIPTION : Verify the Manage Services - Add Blocked Service should not accept rule which has Start time greater than End Time");
			LOGGER.info(
					"STEP 17: ACTION : Enter keyword,  Select 'Block Always' as 'No' and select 'Start From' greater than 'End On' time and click Save Button");
			LOGGER.info(
					"STEP 17: EXPECTED : Rule should not be save instead error should be thrown as 'Start time should be smaller than End time !' ");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to validate 'Manage Services - Add Blocked Service' rule with Start time greater than End Time";
			status = BroadBandCommonPage.addParentalControlInvalidRule(tapEnv, BroadBandTestConstants.MANAGED_SERVICES);
			if (status) {
				LOGGER.info(
						"STEP 17: ACTUAL : Rule is not saved, instead error is successfully thrown as 'Start time should be smaller than End time !'");
			} else {
				LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, true);

			stepNum = "s18";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 18: DESCRIPTION : Verify the Managed Services feature can be disabled");
			LOGGER.info("STEP 18: ACTION : Click on \"Disable\" button in Managed Service page");
			LOGGER.info("STEP 18: EXPECTED :  Managed Services should be successfully enabled");
			LOGGER.info("**********************************************************************************");
			errorMessage = "Unable to disable Managed Services feature !";
			status = BroadBandCommonPage.enableOrDisableRadioButton(tapEnv,
					BroadBandWebGuiElements.ELEMENT_XPATH_PARENTAL_CONTROL_MANAGED_SERVICE_DISABLE);
			if (status) {
				LOGGER.info("STEP 18: ACTUAL : Parental Control - Managed Services is successfully disabled!");
			} else {
				LOGGER.error("STEP 18: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
					status, errorMessage, false);

		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		}

		LOGGER.info("ENDING TEST CASE: TC-RDKB-PARE-CTRL-NEG-5001");
	}

	/**
	 * Method to verify setting Ethernet client ip as static ip and verify internet
	 * connectivity
	 * 
	 * <ol>
	 * <li>Verify obtaining a Ethernet client associated with the gateway.</li>
	 * <li>Verify the correct Ipv4 address for LAN Client.</li>
	 * <li>Verify the correct Ipv6 address for LAN Client.</li>
	 * <li>Verify the internet connectivity in the connected LAN client.</li>
	 * <li>Verify retrieving the host name & Ipv4 address of wireless client.</li>
	 * <li>Verify launching BroadBand Lan UI login page and verify and login
	 * status.</li>
	 * <li>Verify navigating to connected device page.</li>
	 * <li>Verify navigating to Connected Devices > Devices > Edit Device.</li>
	 * <li>Verify adding client ip as static ip.</li>
	 * <li>Verify the internet connectivity in the connected LAN client after
	 * setting client ip as static.</li>
	 * <li>Verify rebooting the device.</li>
	 * <li>Verify client ip is set as static ip or not.</li>
	 * </ol>
	 * 
	 * @param device{@link Dut}
	 * 
	 * @author Prashant Mishra
	 * 
	 * @Refactor Sruthi Santhosh
	 */
	@Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true)
	@TestDetails(testUID = "TC-RDKB-WEBUI-8028")
	public void testToValidateSettingEClientIpAsStatic(Dut device) {
		// Variable Declaration begins
		String testCaseId = "TC-RDKB-WEBUI-028";
		String stepNum = "";
		String errorMessage = "";
		boolean status = false;
		Dut connectedClientSettop = null;
		// Variable Declaration Ends

		LOGGER.info("#######################################################################################");
		LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBUI-8028");
		LOGGER.info("TEST DESCRIPTION: Verify setting LAN Client ip as static ip and verify internet connectivity.");
		LOGGER.info("TEST STEPS : ");
		LOGGER.info("1. Verify connecting a LAN client associated with the gateway.");
		LOGGER.info("2. Verify the correct Ipv4 address for LAN Client.");
		LOGGER.info("3. Verify the correct Ipv6 address for LAN Client.");
		LOGGER.info("4. Verify the internet connectivity in the connected LAN client.");
		LOGGER.info("5. Verify retrieving the host name & Ipv4 address of LAN client.");
		LOGGER.info("6. Verify launching BroadBand Lan UI login page and verify and login status.");
		LOGGER.info("7. Verify navigating to connected device page.");
		LOGGER.info("8. Verify navigating to Connected Devices > Devices > Edit Device.");
		LOGGER.info("9. Verify adding client ip as static ip.");
		LOGGER.info(
				"10. Verify the internet connectivity in the connected LAN client after setting client ip as static.");
		LOGGER.info("11. Verify rebooting the device.");
		LOGGER.info("12. Verify client ip is set as static ip or not.");

		LOGGER.info("#######################################################################################");

		try {
			stepNum = "s1";
			errorMessage = "Unable to obtain LAN client.";
			status = false;
			LOGGER.info("**********************************************************************************");
			LOGGER.info("STEP 1: DESCRIPTION : Verify obtaining a LAN client associated with the gateway.");
			LOGGER.info("STEP 1: ACTION : Obtain one of the associated LAN client.");
			LOGGER.info("STEP 1: EXPECTED : One of the associated LAN client should be obtained successfully.");
			LOGGER.info("**********************************************************************************");
			try {
				connectedClientSettop = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv, device);
			} catch (Exception e) {
				errorMessage = e.getMessage();
				LOGGER.error(errorMessage);
			}
			status = null != connectedClientSettop;
			if (status) {
				LOGGER.info("STEP 1: ACTUAL : One of associated LAN client obtained successfully.");
			} else {
				LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
			}
			LOGGER.info("**********************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

			// step-2 to step-4
			BroadBandConnectedClientUtils.validateIpAddressesAndInternetConnectivityOfConnectedClient(device,
					connectedClientSettop, tapEnv, BroadBandTestConstants.CONSTANT_2, testCaseId);

			/** Common method for verify setting client ip as static ip */
			methodToVerifySettingClientIpAsStaticForAllClients(device, connectedClientSettop, tapEnv, testCaseId,
					BroadBandTestConstants.INTEGER_VALUE_5, BroadBandTestConstants.CLIENT_TYPE_ETH);
		} catch (Exception e) {
			errorMessage = errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
			CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
					false);
		}
		LOGGER.info("ENDING TEST CASE: TC-RDKB-WEBUI-8028");
	}

	/**
	 * Common method to verify setting client ip as static ip for both client type
	 * (Ethernet & WiFI)
	 * 
	 * @param device                Dut instance
	 * @param connectedClientSettop Connected client device
	 * @param tapEnv                AutomaticsTapApi instance
	 * @param testCaseId            Test case id for which method is being called
	 * 
	 * @author Prashant Mishra
	 * 
	 * @Refactor Sruthi Santhosh
	 */
	public static void methodToVerifySettingClientIpAsStaticForAllClients(Dut device, Dut connectedClientSettop,
			AutomaticsTapApi tapEnv, String testCaseId, int stepNumber, String clientType) {
		// Variable declararion starts
		String ipv4AddressRetrievedAfterReboot = "";
		String ipv4AddressRetrievedBeforeReboot = "";
		String hostName = null;
		String stepNum = "";
		String errorMessage = "";
		String macAddressOfWirelessClientOrginal = "";
		boolean status = false;
		WebDriver webDriver = null;
		LanSidePageNavigation lanSidePageNavigation = null;
		BroadBandResultObject result = null;
		// Variable declaration ends

		stepNum = "s" + stepNumber;
		errorMessage = "Unable to get Ipv4 address of connected client.";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ ": DESCRIPTION : Verify retrieving the host name & Ipv4 address of connected client.");
		LOGGER.info("STEP " + stepNumber
				+ ": ACTION : Execute WebPA request for the parameter \"Device.Hosts.Host.{i}.HostName\"");
		LOGGER.info("STEP " + stepNumber
				+ ": EXPECTED : host name & Ipv4 address of connected client should be obtained successfully.");
		LOGGER.info("**********************************************************************************");
		try {
			ipv4AddressRetrievedBeforeReboot = BroadBandConnectedClientUtils.getIpv4AddressFromConnClient(tapEnv,
					device, connectedClientSettop);
			LOGGER.info("ipv4AddressRetrievedBeforeReboot: " + ipv4AddressRetrievedBeforeReboot);
			if (CommonMethods.isNotNull(ipv4AddressRetrievedBeforeReboot)) {
				errorMessage = "Unable to get host name  and phy address of connected client.";
				hostName = BroadBandCommonUtils.getConnectedClientDetailUsingHostIpAddress(device, tapEnv,
						ipv4AddressRetrievedBeforeReboot, BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_HOST_HOSTNAME);
				LOGGER.info("HOST NAME beofre removal of special char: " + hostName);
				hostName = hostName.replace(BroadBandTestConstants.CHARACTER_HYPHEN,
						BroadBandTestConstants.EMPTY_STRING);
				LOGGER.info("HOST NAME after removal of special char: " + hostName);
				macAddressOfWirelessClientOrginal = BroadBandCommonUtils.getConnectedClientDetailUsingHostIpAddress(
						device, tapEnv, ipv4AddressRetrievedBeforeReboot,
						BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_HOST_PHYSICAL_ADDRESS);
				status = CommonMethods.isNotNull(hostName)
						&& CommonMethods.isNotNull(macAddressOfWirelessClientOrginal);
			}

		} catch (Exception e) {
			LOGGER.error("Exception occured while getting the host name and Ip address of the connected client."
					+ errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ ": ACTUAL : Successfully retrieved the host name, Phy address and Ipv4 address of the connected client.");
		} else {
			LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

		stepNumber++;
		stepNum = "s" + stepNumber;
		errorMessage = "Unable to launch and login LAN UI page.";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ ": DESCRIPTION : Verify launching BroadBand Lan UI login page and verify and login status.");
		LOGGER.info("STEP " + stepNumber
				+ ": ACTION : Launch the Web GUI Lan page URL and login with correct credentials.");
		LOGGER.info(
				"STEP " + stepNumber + ": EXPECTED : Login page should be launched and login should be successful.");
		LOGGER.info("**********************************************************************************");
		try {
			status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, connectedClientSettop);
			webDriver = LanWebGuiLoginPage.getDriver();
			lanSidePageNavigation = new LanSidePageNavigation(webDriver);
		} catch (Exception e) {
			errorMessage = "Exception occured while launching and login Lan UI login page." + errorMessage
					+ e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + ": ACTUAL : Login Lan admin page launched and logged in successfully.");
		} else {
			LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, true);

		stepNumber++;
		stepNum = "s" + stepNumber;
		errorMessage = "Unable to navigate to Connected device page.";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify navigating to connected device page.");
		LOGGER.info("STEP " + stepNumber + ": ACTION : Navigate to Connected device page.");
		LOGGER.info(
				"STEP " + stepNumber + ": EXPECTED : Navigation should be  successfull to Connected devices pages.");
		LOGGER.info("**********************************************************************************");
		try {
			status = lanSidePageNavigation.navigateToConnectedDevicesPage(device, tapEnv, webDriver);
		} catch (Exception e) {
			errorMessage = "Exception occured while navigating to connected device page." + errorMessage
					+ e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + ": ACTUAL : Navigated successfully to Connected device page.");
		} else {
			LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, true);

		stepNumber++;
		stepNum = "s" + stepNumber;
		errorMessage = "Unable to navigate to Connected Devices > Devices > Edit Device page.";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ ": DESCRIPTION : Verify navigating to Connected Devices > Devices > Edit Device.");
		LOGGER.info("STEP " + stepNumber + ": ACTION : Click on edit button for connected client.");
		LOGGER.info("STEP " + stepNumber
				+ ": EXPECTED : Navigation should be successful to Connected Devices > Devices > Edit Device page.");
		LOGGER.info("**********************************************************************************");
		try {
			status = LanSideBasePage.isPageLaunchedByUsingWebElementForConnectedDevicesEditDevice(device, tapEnv,
					By.xpath(BroadBandWebGuiElements.XPATH_ADD_DEVICE_WITH_RESERVED_IP),
					BroadBandTestConstants.ADD_DEVICE_WITH_RESERVED_IP);
		} catch (Exception e) {
			errorMessage = "Exception occured while navigating to Connected Devices > Devices > Edit Device page."
					+ errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ ": ACTUAL : Navigated successfully to Connected Devices > Devices > Edit Device page.");
		} else {
			LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, true);

		stepNumber++;
		stepNum = "s" + stepNumber;
		errorMessage = "Unable to add client ip as static ip.";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify adding client ip as static ip.");
		LOGGER.info("STEP " + stepNumber
				+ ": ACTION : Click on reserved ip radio button and enter client ip address in text box.");
		LOGGER.info(
				"STEP " + stepNumber + ": EXPECTED : Client ip should be added as static ip from Lan Admin GUI page.");
		LOGGER.info("**********************************************************************************");
		try {

			String pageTitleKeyword = BroadbandPropertyFileHandler.getPageTitleForConnectedDevicesEditDevicePage();

			LOGGER.info("pageTitleKeyword: " + pageTitleKeyword);
			status = BroadBandCommonPage.configureReservedIpAddress(hostName, macAddressOfWirelessClientOrginal,
					ipv4AddressRetrievedBeforeReboot, tapEnv, pageTitleKeyword, webDriver);

		} catch (Exception e) {
			errorMessage = "Exception occured while adding client ip as static ip." + errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + ": ACTUAL : Client ip added successfully as static ip.");
		} else {
			LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum, status,
				errorMessage, true);

		stepNumber++;
		stepNum = "s" + stepNumber;
		errorMessage = "Unable to  verify internet connectivity in the connected client after setting client ip as static.";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber
				+ ": DESCRIPTION : Verify the internet connectivity in the connected wifi client after setting client ip as static.");
		LOGGER.info("STEP " + stepNumber
				+ ": ACTION : Execute following command in connected client: curl -4 -f --interface <interfaceName> www.google.com | grep \"200 OK\" OR ping -4 -n 5 google.com");
		LOGGER.info("STEP " + stepNumber
				+ ": EXPECTED : The internet connectivity must be successful in the connected client after changing client ip to static.");
		LOGGER.info("**********************************************************************************");
		try {
			ipv4AddressRetrievedBeforeReboot = BroadBandConnectedClientUtils.getIpv4AddressFromConnClient(tapEnv,
					device, connectedClientSettop);
			LOGGER.info("ipv4AddressRetrievedBeforeReboot: " + ipv4AddressRetrievedBeforeReboot);
			long startTime = System.currentTimeMillis();
			do {
				result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
						connectedClientSettop, BroadBandTestConstants.URL_WIKIPEDIA,
						BroadBandTestConstants.EMPTY_STRING);
				status = result.isStatus();
				errorMessage = result.getErrorMessage();
			} while (!status && (System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
					&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
		} catch (Exception e) {
			errorMessage = "Exception occured while verifying nternet connectivity in the connected wifi client after setting client ip as static.."
					+ errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ ": ACTUAL : Internet connectivity verified successfully after setting client ip as static.");
		} else {
			LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

		stepNumber++;
		stepNum = "s" + stepNumber;
		errorMessage = "Unable to reboot the device.";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify rebooting the device.");
		LOGGER.info("STEP " + stepNumber + ": ACTION : Reboot the device and wait for device to come up.");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : Device should be rebooted and comes up with all processes.");
		LOGGER.info("**********************************************************************************");
		try {
			status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
		} catch (Exception e) {
			errorMessage = "Exception occured while rebooting the device." + errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber
					+ ": ACTUAL : Device rebooted successfully and came up with all the processes.");
		} else {
			LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

		stepNumber++;
		stepNum = "s" + stepNumber;
		errorMessage = "Failed to reconnect client to 2.4GHz WiFi.";
		status = false;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify client ip is set as static ip or not.");
		LOGGER.info("STEP " + stepNumber + ": ACTION : Get client ipv4 address and compare with ip got before reboot,");
		LOGGER.info("STEP " + stepNumber + ": EXPECTED : Ip address before reboot and after reboot should be same.");
		LOGGER.info("**********************************************************************************");
		try {
			if (clientType.equals(BroadBandTestConstants.CLIENT_TYPE_WIFI)) {
				result = BroadBandConnectedClientUtils.connectGivenConnectedClientToWifi(device, tapEnv,
						connectedClientSettop, WiFiFrequencyBand.WIFI_BAND_2_GHZ);
				if (!result.isStatus()) {
					errorMessage = "Failed to reconnect client to 5GHz WiFi.";
					result = BroadBandConnectedClientUtils.connectGivenConnectedClientToWifi(device, tapEnv,
							connectedClientSettop, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
				}
				if (result.isStatus()) {
					errorMessage = "Unable to verify the static ip address of client.";
					ipv4AddressRetrievedAfterReboot = BroadBandConnectedClientUtils.getIpv4AddressFromConnClient(tapEnv,
							device, connectedClientSettop);
					LOGGER.info("ipv4AddressRetrievedAfterReboot: " + ipv4AddressRetrievedAfterReboot);
					status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
							ipv4AddressRetrievedBeforeReboot, ipv4AddressRetrievedAfterReboot);
				}
			} else {
				errorMessage = "Unable to verify the static ip address of client.";
				ipv4AddressRetrievedAfterReboot = BroadBandConnectedClientUtils.getIpv4AddressFromConnClient(tapEnv,
						device, connectedClientSettop);
				LOGGER.info("ipv4AddressRetrievedAfterReboot: " + ipv4AddressRetrievedAfterReboot);
				status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
						ipv4AddressRetrievedBeforeReboot, ipv4AddressRetrievedAfterReboot);
			}
		} catch (Exception e) {
			errorMessage = "Exception occured while verifying static ip of the client." + errorMessage + e.getMessage();
			LOGGER.error(errorMessage);
		}
		if (status) {
			LOGGER.info("STEP " + stepNumber + ": ACTUAL : Client ip verified successfully as static ip.");
		} else {
			LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	}
}
