/*
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

import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.support.ui.Select;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.ConnectedNattedClientsUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.rdkb.webui.constants.BroadBandWebGuiElements;
import com.automatics.rdkb.webui.constants.BroadBandWebGuiTestConstant;
import com.automatics.rdkb.webui.page.BroadBandCommonPage;
import com.automatics.rdkb.webui.page.BroadbandLocalIpConfigurationPage;
import com.automatics.rdkb.webui.page.LanSideBasePage;
import com.automatics.rdkb.webui.page.LanSidePageNavigation;
import com.automatics.rdkb.webui.page.LanSideWiFiPage;
import com.automatics.rdkb.webui.page.LanWebGuiLoginPage;
import com.automatics.rdkb.webui.utils.BroadBandWebUiUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaParameter;

public class LanPageWebGuiTests extends AutomaticsTestBase {
    
    /** String for step number */
    public int stepNumber = 0;
    
    public String stepNum = "S1";

    public boolean isBrowserOpen = false;

    /**
     * 
     * <ol>
     * <li>1:Login to Lan side GUI page of device</li>
     * <li>2:Launch Connection page and navigate to WIFI page</li>
     * <li>3:Navigate to Edit 2.4Ghz page and verfiy channel bandwidth options 20MHz and 20/40MHz</li>
     * <li>4:Change Wi-Fi settings of 2.4Ghz SSID Wi-FI settings(SSID, Password, Channel Bandwidth-40MHz)</li>
     * <li>5:Validate Wifi SSID name, Password and channel bandwidth using webpa</li>
     * <li>6:Connect to a connected client with 2.4Ghz radio ssid</li>
     * <li>7:Verify whether Connected client got the IPv4 address</li>
     * <li>8:Verify whether Connected client got the IPv6 address.</li>
     * <li>9:Verify whether you have connectivity using that particular interface using IPV4</li>
     * <li>10:Verify whether you have connectivity using that particular interface using IPV6</li>
     * <li>11:Change Wi-Fi settings of 2.4Ghz SSID Wi-FI settings(SSID, Password, Channel Bandwidth-20MHz)</li>
     * <li>12:Validate Wifi SSID name , Password and channel bandwidth using webpa</li>
     * <li>13:Connect to a connected client with 2.4Ghz radio ssid</li>
     * <li>14:Verify whether Connected client got the IPv4 address</li>
     * <li>15:Verify whether Connected client got the IPv6 address.</li>
     * <li>16:Verify whether you have connectivity using that particular interface using IPV4</li>
     * <li>17:Verify whether you have connectivity using that particular interface using IPV6</li>
     * </ol>
     * 
     * @param device
     * @Author PRASANTH REDDY
     * @refactor Athira
     */

    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, groups = {
	    TestGroup.WEBPA, TestGroup.WIFI }, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-CHANNEL-BANDWIDTH-1001")
    public void verifyChannelBandwidthInAdminGUI24Ghz(Dut device) {
	boolean status = false;// String to store the test case status
	String testId = "TC-RDKB-CHANNEL-BANDWIDTH-101";// String to store Testcase id
	String testStep = null;// Test step number
	String errorMessage = null;// String to store the error message
	WebDriver lanDriver = null;// Webdriver object
	Dut clientDut = null;// Dut object to store client device
	String wifi24GhzSsidName = null;// String to store wifi SSId name
	String wifi24GhzPassPhrase = null;// String to store wifi passphrase
	String defaultChannel = null;// String to store channel bandwidth
	String meshInitialStatus = null;
	boolean isBusinessClsDevice = DeviceModeHandler.isBusinessClassDevice(device);
	try {
	    LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-CHANNEL_BANDWIDTH-1001#####################");
	    LOGGER.info("TEST DESCRIPTION: Test to verify channel bandwidth of 2.4Ghz SSID from LAN GUI");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("PRE-CONDITION : ");
	    LOGGER.info("1.Login to Lan side GUI page of device");
	    LOGGER.info("2.Launch Connection page and navigate to WIFI page");
	    LOGGER.info("3.Navigate to Edit 2.4Ghz page and verfiy channel bandwidth options 20MHz and 20/40MHz");
	    LOGGER.info(
		    "4.Change Wi-Fi settings of 2.4Ghz SSID Wi-FI settings(SSID, Password, Channel Bandwidth-40MHz)");
	    LOGGER.info("5.Validate Wifi SSID name , Password and channel bandwidth using webpa");
	    LOGGER.info("6.Connect to a connected client with 2.4Ghz radio ssid ");
	    LOGGER.info("7.Verify whether Connected client  got the  IPv4  address ");
	    LOGGER.info("8.Verify whether Connected client got the   IPv6 address.");
	    LOGGER.info("9.Verify whether you have connectivity using that particular interface using IPV4");
	    LOGGER.info("10.Verify whether you have connectivity using that particular interface using IPV6");
	    LOGGER.info(
		    "11.Change Wi-Fi settings of 2.4Ghz SSID Wi-FI settings(SSID, Password, Channel Bandwidth-20MHz)");
	    LOGGER.info("12.Validate Wifi SSID name , Password and channel bandwidth using webpa");
	    LOGGER.info("13.Connect to a connected client with 2.4Ghz radio ssid ");
	    LOGGER.info("14.Verify whether Connected client  got the IPv4 address ");
	    LOGGER.info("15.Verify whether Connected client got the IPv6 address.");
	    LOGGER.info("16.Verify whether you have connectivity using that particular interface using IPV4");
	    LOGGER.info("17.Verify whether you have connectivity using that particular interface using IPV6");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("################################# STARTING PRE-CONFIGURATIONS #############################");
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRE-CONDITION 1: Verify whether Private SSID 2.4Ghz can be enabled using webPA");
	    LOGGER.info("PRE-CONDITION 1: EXPECTED: Private SSID 2.4Ghz should be enabled successfully");
	    errorMessage = "Unable to enable 2.4Ghz private SSID using webpa";
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE,
		    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

	    if (status) {
		LOGGER.info("PRE-CONDITION 1: ACTUAL : PRE CONDITION EXECUTED SUCCESSFULLY");
	    } else {
		LOGGER.error("PRE-CONDITION 1: ACTUAL : " + errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRE-CONDITION 2: Retrieve WiFi SSID name of 2.4Ghz using webpa");
	    LOGGER.info("PRE-CONDITION 2: EXPECTED: Private SSID 2.4Ghz name should be retrieved");
	    errorMessage = "Unable to retrieve 2.4Ghz private SSID name using webpa";
	    wifi24GhzSsidName = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME);

	    if (CommonMethods.isNotNull(wifi24GhzSsidName)) {
		LOGGER.info("PRE-CONDITION 2: ACTUAL : PRE CONDITION EXECUTED SUCCESSFULLY");
	    } else {
		LOGGER.error("PRE-CONDITION 2: ACTUAL : " + errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRE-CONDITION 3: Retrieve WiFi SSID Passphrase of 2.4Ghz using webpa");
	    LOGGER.info("PRE-CONDITION 3: EXPECTED: Private SSID 2.4Ghz Passphrase should be retrieved");
	    errorMessage = "Unable to retrieve 2.4Ghz private SSID Passphrase using webpa";
	    wifi24GhzPassPhrase = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2GHZ_SECURITY_KEYPASSPHRASE);

	    if (CommonMethods.isNotNull(wifi24GhzPassPhrase)) {
		LOGGER.info("PRE-CONDITION 3: ACTUAL : PRE CONDITION EXECUTED SUCCESSFULLY");
	    } else {
		LOGGER.error("PRE-CONDITION 3: ACTUAL : " + errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("PRE-CONDITION 4: Retrieve WiFi SSID Channel bandwidth of 2.4Ghz using webpa");
	    LOGGER.info("PRE-CONDITION 4: EXPECTED: Private SSID 2.4Ghz Channel bandwidth should be retrieved");
	    errorMessage = "Unable to retrieve 2.4Ghz private SSID Channel bandwidth using webpa";
	    defaultChannel = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_OPERATING_BANDWIDTH_IN_2GHZ_BAND);

	    if (CommonMethods.isNotNull(defaultChannel)) {
		LOGGER.info("PRE-CONDITION 4: ACTUAL : PRE CONDITION EXECUTED SUCCESSFULLY");
	    } else {
		LOGGER.error("PRE-CONDITION 4: ACTUAL : " + errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }
	    if (!isBusinessClsDevice) {
		meshInitialStatus = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_RDKCENTRAL_MESH_ENABLE);
		BroadBandPreConditionUtils.executePreConditionToDisableMeshUsingSystemCommands(device, tapEnv,
			meshInitialStatus, BroadBandTestConstants.CONSTANT_5);
	    }
	    /**
	     * STEP 1: Launch the Login page on Connected Client
	     */
	    status = false;
	    testStep = "s1";
	    errorMessage = "Connected client device is not obtained";
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STEP 1: DESCRIPTION :LAUNCH ADMIN LOGIN PAGE AND LOGIN");
	    LOGGER.info("STEP 1: ACTION :LOGIN TO LAN SIDE GUI ADMIN PAGE WITH 'admin/password'");
	    LOGGER.info("STEP 1: EXPECTED: LOGIN SHOULD BE SUCCESSFUL AND REDIRECTED TO At a Glance Page");
	    LOGGER.info("#######################################################################################");
	    try {
		clientDut = BroadBandConnectedClientUtils
			.get2GhzOr5GhzWiFiCapableClientDeviceAndConnectToCorrespondingSsid(device, tapEnv);
		if (null != clientDut) {
		    errorMessage = "Unable to Login to LanGUI page using Admin credential";
		    status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, clientDut);
		    lanDriver = LanWebGuiLoginPage.getDriver();
		}
		if (status) {
		    LOGGER.info("STEP 1:ACTUAL:Launched GUI page and login sucessfully");
		} else {
		    LOGGER.error("STEP 1:ACTUAL:" + errorMessage);
		}
	    } catch (Exception exception) {
		LOGGER.error("Exception in launching GUI page " + exception.getMessage());
	    }
	    LOGGER.info("#######################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);
	    /**
	     * STEP 2:Launch Connection page and navigate to WIFI page
	     */
	    status = false;
	    testStep = "s2";
	    errorMessage = "Unable to navigate to Connection page -> WiFi page from 'At a Glance Page'";
	    LOGGER.info("##########################################################################");
	    LOGGER.info("STEP 2 : DESCRIPTION :LAUNCH CONNECTION PAGE AND NAVIGATE TO WIFI PAGE");
	    LOGGER.info("STEP 2 : ACTION :NAVIGATE TO WIFI PAGE FROM CONNECTION PAGE");
	    LOGGER.info("STEP 2 : EXPECTED:Connection page -> WiFi page  NAVIGATE SUCCESSFULLY");
	    LanSideWiFiPage lanSideWiFiPage = new LanSideWiFiPage(lanDriver);
	    try {
		if (lanSideWiFiPage.navigateToConnectionFromGateway(device, tapEnv)) {
		    status = lanSideWiFiPage.navigateToWiFiPage(device, tapEnv);
		}
	    } catch (Exception exception) {
		LOGGER.error("Exception in launching WiFi page " + exception.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 2:ACTUAL :Navigate to WiFi page successfully verified");
	    } else {
		LOGGER.error("STEP 2:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);
	    /**
	     * STEP 3:Navigate to Edit 2.4Ghz page and Verify channel bandwidth options 20MHz and 20/40MHz
	     */
	    status = false;
	    testStep = "s3";
	    errorMessage = "Unable to verify channel bandwidth options in 2.4Ghz WiFi Page";
	    LOGGER.info("##########################################################################");
	    LOGGER.info("STEP 3 : DESCRIPTION :  VERIFY CHANNEL BANDWIDTH OPTIONS 20MHZ AND 20/40MHZ");
	    LOGGER.info("STEP 3 : ACTION : NAVIGATE TO 2.4GHZ EDIT PAGE");
	    LOGGER.info("STEP 3 : EXPECTED : CHANNEL BANDWIDTH OPTIONS SHOULD BE VERIFIED");
	    BroadBandResultObject broadBandResultObject = new BroadBandResultObject();
	    try {
		broadBandResultObject = lanSideWiFiPage.verifyBandwidthOptionsInWifiPage(device, tapEnv, false);
		errorMessage = broadBandResultObject.getErrorMessage();
	    } catch (Exception e) {
		LOGGER.error("Exception in Validating Bandwith options in Wifi Edit page \n" + e.getMessage());
	    }
	    if (broadBandResultObject.isStatus()) {
		status = true;
		LOGGER.info("STEP 3:ACTUAL :Channel bandwidth options in Wifi  page verified successfully");
	    } else {
		LOGGER.error("STEP 3:ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("##########################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);
	    // Webpa parameters for Wifi 2.4Ghz SSID
	    String[] wifi24GhzParamters = { BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_NAME,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2GHZ_SECURITY_KEYPASSPHRASE,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_OPERATING_BANDWIDTH_IN_2GHZ_BAND };
	    // Validation for Channel Bandwidth 40MHz
	    status = false;
	    /**
	     * STEP 3 to STEP 10
	     */
	    channelBandwidthOptionsValidation(device, clientDut, lanSideWiFiPage, testId, lanDriver,
		    BroadBandTestConstants.INTEGER_VALUE_4, BroadBandTestConstants.STRING_VALUE_24GHZ_SSID1,
		    BroadBandTestConstants.STRING_VALUE_24_5GHZ_KEY1, BroadBandTestConstants.OPERATING_BANDWIDTH_40_MMZ,
		    wifi24GhzParamters, false, BroadBandTestConstants.WiFiFrequencyBand.WIFI_BAND_2_GHZ,
		    BroadBandWebGuiElements.ELEMENT_ID_CHANNEL_40MHZ);

	    // Validation for Channel Bandwidth 20MHz
	    status = false;

	    /**
	     * STEP 11 to STEP 17
	     */
	    channelBandwidthOptionsValidation(device, clientDut, lanSideWiFiPage, testId, lanDriver,
		    BroadBandTestConstants.INTEGER_VALUE_11, BroadBandTestConstants.STRING_VALUE_24GHZ_SSID2,
		    BroadBandTestConstants.STRING_VALUE_24_5GHZ_KEY2, BroadBandTestConstants.OPERATING_BANDWIDTH_20_MMZ,
		    wifi24GhzParamters, false, BroadBandTestConstants.WiFiFrequencyBand.WIFI_BAND_2_GHZ,
		    BroadBandWebGuiElements.ELEMENT_ID_CHANNEL_20MHZ);
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception occured in channel bandwidth validation" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStep, status, errorMessage, true);
	} finally {
	    try {
		LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
		BroadBandWebGuiTests broadBandWebGuiTests = new BroadBandWebGuiTests();
		broadBandWebGuiTests.postConfigurationsForWifiRestoration(device, WiFiFrequencyBand.WIFI_BAND_2_GHZ,
			clientDut, wifi24GhzSsidName, wifi24GhzPassPhrase, defaultChannel);
	    } catch (Exception e) {
		LOGGER.error("Exception in POST CONFIGURATIONS " + e.getMessage());
	    }
	}
	LOGGER.info("#################### ENDING TEST CASE: TC-RDKB-CHANNEL_BANDWIDTH-1001#####################");
    }

    public void channelBandwidthOptionsValidation(Dut device, Dut clientDevice, LanSideWiFiPage lanSideWiFiPage,
	    String testId, WebDriver lanDriver, Integer testStepNumber, String wifiSsidName, String wifiSsidPassword,
	    String channelBandwithValue, String[] wifiRadioParamters, boolean is5Ghz,
	    RDKBTestConstants.WiFiFrequencyBand wiFiFrequencyBand, String bandwidthOption) {
	boolean status = false;// Execution status
	String testStep;// Test step number
	String errorMessage;// String to store error message
	status = false;
	testStep = "s" + testStepNumber;
	errorMessage = "Unable to navigate to " + wiFiFrequencyBand + " WiFi edit page";
	LOGGER.info("##########################################################################");
	LOGGER.info("STEP " + testStepNumber + " : DESCRIPTION : NAVIGATE TO EDIT " + wiFiFrequencyBand
		+ " PAGE AND CHANGE WI-FI SETTINGS");
	LOGGER.info("STEP " + testStepNumber + " : ACTION : NAVIGATE TO EDIT " + wiFiFrequencyBand + " PAGE");
	LOGGER.info("STEP " + testStepNumber + " : EXPECTED : WIFI SETTINGS SHOULD BE SAVED SUCCESSFULLY");
	BroadBandResultObject broadBandResultObject = new BroadBandResultObject();
	try {
	    // BroadBandWebPaUtils.disableAkerCloudUiAndMeshUsingWebpa(device, tapEnv);
	    broadBandResultObject = lanSideWiFiPage.editWifiPageAndSaveSettings(device, tapEnv, lanDriver, wifiSsidName,
		    wifiSsidPassword, is5Ghz, bandwidthOption);
	    errorMessage = broadBandResultObject.getErrorMessage();
	} catch (Exception exception) {
	    LOGGER.error("Exception in launching " + wiFiFrequencyBand + " edit page " + exception.getMessage());
	}
	if (broadBandResultObject.isStatus()) {
	    status = true;
	    LOGGER.info("STEP " + testStepNumber + ":ACTUAL :Navigated to Edit " + wiFiFrequencyBand
		    + " page and successsfully changed settings");
	} else {
	    LOGGER.error("STEP " + testStepNumber + ":ACTUAL :" + errorMessage);
	}
	LOGGER.info("##########################################################################");
	tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);

	testStepNumber++;
	status = false;
	testStep = "s" + testStepNumber;
	errorMessage = "UNABLE TO VERIFY " + wiFiFrequencyBand + " WIFI RADIO SSID , PASSWORD ,CHANNEL BANDWIDTH";
	LOGGER.info("##########################################################################");
	LOGGER.info("STEP " + testStepNumber + " : DESCRIPTION :VALIDATE WIFI " + wiFiFrequencyBand
		+ " SSID NAME AND PASSWORD USING WEBPA");
	LOGGER.info("STEP " + testStepNumber
		+ " : ACTION :EXECUTE WEBPA COMMAND WITH PARAMETER SSID,PASSPHRASE AND CHANNEL BANDWIDTH");
	LOGGER.info("STEP " + testStepNumber
		+ " : EXPECTED:WIFI SSID NAME , PASSWORD ,CHANNEL BANDWIDTH SHOULD BE REFLECTED SUCCESSFULLY");
	List<String> output24GhzSsidValues = tapEnv.executeWebPaCommands(device, wifiRadioParamters);
	if (output24GhzSsidValues.contains(wifiSsidName) && output24GhzSsidValues.contains(wifiSsidPassword)
		&& output24GhzSsidValues.contains(channelBandwithValue)) {
	    status = true;
	    LOGGER.info("STEP " + testStepNumber
		    + ":ACTUAL :WIFI SSID name,passphrase and channel bandwidth verified successfully");
	} else {
	    LOGGER.error("STEP " + testStepNumber + ":ACTUAL :" + errorMessage);
	}
	LOGGER.info("##########################################################################");
	tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);

	testStepNumber++;
	status = false;
	testStep = "s" + testStepNumber;
	errorMessage = "Unable to connect to " + wiFiFrequencyBand + " SSID";
	LOGGER.info("##########################################################################");
	LOGGER.info("STEP " + testStepNumber + " : DESCRIPTION :CONNECT TO A CONNECTED CLIENT WITH " + wiFiFrequencyBand
		+ " RADIO SSID ");
	LOGGER.info("STEP " + testStepNumber + " : ACTION :CONNECT TO " + wiFiFrequencyBand + " SSID");
	LOGGER.info("STEP " + testStepNumber + " : EXPECTED:CONNECTION SHOULD BE SUCCESFUL");

	BroadBandResultObject bandResultObject = BroadBandConnectedClientUtils.connectGivenConnectedClientToWifi(device,
		tapEnv, clientDevice, wiFiFrequencyBand);
	lanDriver.navigate().refresh();
	tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
	lanDriver.navigate().refresh();
	String url = DeviceModeHandler.isBusinessClassDevice(device)
		? BroadBandTestConstants.STRING_BUSINESS_CLASS_GATEWAYIP
		: BroadBandTestConstants.STRING_RESIDENTIAL_CLASS_GATEWAYIP;
	// Validating whether page is wifi page or redirect to wifi page
	String pageTitle = lanDriver.getTitle();
	LOGGER.info("Current Page title in GUI- " + pageTitle);
	if (pageTitle.equals(url)) {
	    lanDriver.navigate().refresh();
	    // lanSideWiFiPage.loginToAdminPageAndRedirectToWifiPage(tapEnv,
	    // settop, clientSettop, lanDriver);
	}
	boolean wifiPageStatus = BroadBandWebUiUtils.validatePageLaunchedStatusWithPageTitle(lanDriver,
		BroadbandPropertyFileHandler.getPageTitleConnectionWiFi());
	if (!wifiPageStatus)
	    wifiPageStatus = lanSideWiFiPage.navigateToWiFiPage(device, tapEnv);

	if (bandResultObject.isStatus() && wifiPageStatus) {
	    status = true;
	    LOGGER.info("STEP " + testStepNumber + " :ACTUAL :Connection establishment to " + wiFiFrequencyBand
		    + " SSID is successful");
	} else {
	    LOGGER.error("STEP " + testStepNumber + " :ACTUAL :" + errorMessage);
	}
	LOGGER.info("##########################################################################");
	tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, true);

	testStepNumber++;
	testStep = "s" + testStepNumber;
	status = false;
	errorMessage = "Interface  didnt get the correct IPV4 address";
	LOGGER.info("#####################################################################################");
	LOGGER.info("STEP " + testStepNumber
		+ ":DESCRIPTION:VERIFY WHETHER THE CLIENT GOT THE IPV4 ADDRESS FROM DHCP RANGE");
	LOGGER.info("STEP " + testStepNumber
		+ ":ACTION : EXECUTE ipconfig/ifconfig AND VALIDATE IPV4 ADDRESS WITH DEVICE DHCP RANGE");
	LOGGER.info("STEP " + testStepNumber + ":EXPECTED:IPV4 ADDRESS SHOULD  BE VALIDATED IN DHCP RANGE");
	LOGGER.info("#####################################################################################");

	status = BroadBandConnectedClientUtils.isConnClientIpv4AddrBtwnDhcpRange(tapEnv, device, clientDevice);
	if (status) {
	    LOGGER.info("STEP " + testStepNumber + ":ACTUAL :Connected client has IPv4 address in DHCP range");
	} else {
	    LOGGER.error("STEP " + testStepNumber + ":ACTUAL :" + errorMessage);
	}
	LOGGER.info("##########################################################################");
	tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);

	testStepNumber++;
	testStep = "s" + testStepNumber;
	status = false;
	errorMessage = "Interface  didnt get the correct IPV6 address";
	LOGGER.info("#####################################################################################");
	LOGGER.info("STEP " + testStepNumber + ":DESCRIPTION:VERIFY WHETHER CLIENT OBTAINED THE IPV6 ADDRESS.");
	LOGGER.info("STEP " + testStepNumber + ":ACTION : EXECUTE ipconfig/ifconfig AND VALIDATE IPV6 ADDRESS ");
	LOGGER.info("STEP " + testStepNumber + ":EXPECTED:IPV6 ADDRESS SHOULD  BE VALIDATED");
	LOGGER.info("#####################################################################################");

	String osType = ((Device) clientDevice).getOsType();
	status = BroadBandConnectedClientUtils.verifyIpv6AddressForWiFiOrLanInterfaceConnectedWithRdkbDevice(osType,
		clientDevice, tapEnv);
	if (status) {
	    LOGGER.info(
		    "STEP " + testStepNumber + ": ACTUAL : Connected client has IPv6 address validated successfully");
	} else {
	    LOGGER.error("STEP " + testStepNumber + ": ACTUAL :" + errorMessage);
	}
	LOGGER.info("##########################################################################");
	tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);

	testStepNumber++;
	testStep = "s" + testStepNumber;
	status = false;
	LOGGER.info("#####################################################################################");
	LOGGER.info("STEP " + testStepNumber
		+ ": DESCRIPTION :VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV4 WITH CURL REQUEST. ");
	LOGGER.info("STEP " + testStepNumber
		+ ": ACTION : EXECUTE curl --connect-timeout 20 --head -4 google.com SHOULD BE SUCCESSFUL");
	LOGGER.info("STEP " + testStepNumber + ": EXPECTED: CONNECTIVITY CHECK SHOULD RETURN STATUS AS 200");
	LOGGER.info("#####################################################################################");
	bandResultObject = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
		clientDevice, BroadBandTestConstants.URL_GOOGLE, BroadBandTestConstants.IP_VERSION4);
	status = bandResultObject.isStatus();
	errorMessage = bandResultObject.getErrorMessage();
	if (status) {
	    LOGGER.info("STEP " + testStepNumber + ": ACTUAL: connectivity successful using ipv4 interface");
	} else {
	    LOGGER.error("STEP " + testStepNumber + ": ACTUAL: " + errorMessage);
	}
	LOGGER.info("#####################################################################################");
	tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);

	testStepNumber++;
	status = false;
	testStep = "s" + testStepNumber;
	LOGGER.info("#####################################################################################");
	LOGGER.info("STEP " + testStepNumber
		+ ": DESCRIPTION :VERIFY CONNECTIVITY OF CONNECTED CLIENT USING IPV6 WITH CURL REQUEST");
	LOGGER.info("STEP " + testStepNumber
		+ ": ACTION :  curl --connect-timeout 20 --head -6 google.com SHOULD BE SUCCESSFUL");
	LOGGER.info("STEP " + testStepNumber + ": EXPECTED: CONNECTIVITY CHECK SHOULD RETURN STATUS AS 200");
	LOGGER.info("#####################################################################################");
	bandResultObject = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
		clientDevice, BroadBandTestConstants.URL_GOOGLE, BroadBandTestConstants.IP_VERSION6);
	status = bandResultObject.isStatus();
	errorMessage = bandResultObject.getErrorMessage();
	if (status) {
	    LOGGER.info("STEP " + testStepNumber
		    + ": ACTUAL:Internet Connectivity successful using ipv6 with Curl request");
	} else {
	    LOGGER.error("STEP " + testStepNumber + ": ACTUAL: " + errorMessage);
	}
	LOGGER.info("#####################################################################################");
	tapEnv.updateExecutionStatus(device, testId, testStep, status, errorMessage, false);

    }

    /**
     * Validate Selecting open risky mode and cancelling it immediately should not gray out password field Ethernet
     * client
     * <ol>
     * <li>Login to Lan Gui using Gateway IP</li>
     * <li>Navigate to wifi page</li>
     * <li>Navigate to wifi 2.4Ghz Configuration page</li>
     * <li>Validate Password Text box is grayed or not</li>
     * <li>Select security mode to Open mode</li>
     * <li>Apply settings and cancel pop up</li>
     * <li>Validate Password Text box is grayed or not</li>
     * <li>Navigate to wifi 5Ghz Configuration page</li>
     * <li>Validate Password Text box is grayed or not</li>
     * <li>Select security mode to Open mode</li>
     * <li>Apply settings and cancel pop up</li>
     * <li>Validate Password Text box is grayed or not</li>
     * </ol>
     * 
     * @param device
     * 
     * @author prasanthreddy.a
     * @refactor Govardhan
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.WIFI)
    @TestDetails(testUID = "TC-RDKB-WIFI-SCRTY-CHECK-1001")
    public void testToVerifyOpnScrtyInEthnt(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WIFI-SCRTY-CHECK-101";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	Dut lanClient = null;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-SCRTY-CHECK-1001");
	LOGGER.info(
		"TEST DESCRIPTION: Validate Selecting open risky mode and cancelling it immediately should not gray out password field Ethernet client");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Login to Lan Gui using Gateway IP");
	LOGGER.info("2. Navigate to wifi page ");
	LOGGER.info("3. Navigate to wifi 2.4Ghz Configuration page");
	LOGGER.info("4. Validate Password Text box is grayed or not");
	LOGGER.info("5. Select security mode to Open mode ");
	LOGGER.info("6. Apply settings and cancel pop up");
	LOGGER.info("7. Validate Password Text box is grayed or not");
	LOGGER.info("8. Navigate to wifi 5Ghz Configuration page");
	LOGGER.info("9. Validate Password Text box is grayed or not");
	LOGGER.info("10. Select security mode to Open mode ");
	LOGGER.info("11. Apply settings and cancel pop up");
	LOGGER.info("12. Validate Password Text box is grayed or not");

	LOGGER.info("#######################################################################################");

	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    lanClient = BroadBandPreConditionUtils.executePreConditionToVerifyLanClientStatus(device, tapEnv);
	    LOGGER.info("**********************************************************************************");
	    /**
	     * Step 1 - Step 12 Validates in helper method
	     */
	    helperMethodToOpnScrty(device, lanClient, tapEnv, testCaseId);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-SCRTY-CHECK-1001");
    }

    /**
     * Validate Selecting open risky mode and cancelling it immediately should not gray out password field Wifi 2.4Ghz
     * client
     * <ol>
     * <li>Login to Lan Gui using Gateway IP</li>
     * <li>Navigate to wifi page</li>
     * <li>Navigate to wifi 2.4Ghz Configuration page</li>
     * <li>Validate Password Text box is grayed or not</li>
     * <li>Select security mode to Open mode</li>
     * <li>Apply settings and cancel pop up</li>
     * <li>Validate Password Text box is grayed or not</li>
     * <li>Navigate to wifi 5Ghz Configuration page</li>
     * <li>Validate Password Text box is grayed or not</li>
     * <li>Select security mode to Open mode</li>
     * <li>Apply settings and cancel pop up</li>
     * <li>Validate Password Text box is grayed or not</li>
     * </ol>
     * 
     * @param device
     * 
     * @author prasanthreddy.a
     * @refactor Govardhan
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.WIFI)
    @TestDetails(testUID = "TC-RDKB-WIFI-SCRTY-CHECK-1002")
    public void testToVerifyOpnScrtyIn24GhzClnt(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WIFI-SCRTY-CHECK-102";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	Dut lanClient = null;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-SCRTY-CHECK-1002");
	LOGGER.info(
		"TEST DESCRIPTION: Validate Selecting open risky mode and cancelling it immediately should not gray out password field Wifi 2.4Ghz client");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Login to Lan Gui using Gateway IP");
	LOGGER.info("2. Navigate to wifi page ");
	LOGGER.info("3. Navigate to wifi 2.4Ghz Configuration page");
	LOGGER.info("4. Validate Password Text box is grayed or not");
	LOGGER.info("5. Select security mode to Open mode ");
	LOGGER.info("6. Apply settings and cancel pop up");
	LOGGER.info("7. Validate Password Text box is grayed or not");
	LOGGER.info("8. Navigate to wifi 5Ghz Configuration page");
	LOGGER.info("9. Validate Password Text box is grayed or not");
	LOGGER.info("10. Select security mode to Open mode ");
	LOGGER.info("11. Apply settings and cancel pop up");
	LOGGER.info("12. Validate Password Text box is grayed or not");

	LOGGER.info("#######################################################################################");

	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    lanClient = BroadBandPreConditionUtils.executePreConditionToVerifyWiFiClientStatus(device, tapEnv,
		    BroadBandTestConstants.BAND_2_4GHZ);

	    LOGGER.info("**********************************************************************************");

	    /**
	     * Step 1 - Step 12 Validates in helper method
	     */
	    helperMethodToOpnScrty(device, lanClient, tapEnv, testCaseId);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-SCRTY-CHECK-1002");
    }

    /**
     * Validate Selecting open risky mode and cancelling it immediately should not gray out password field Wifi 5Ghz
     * clients
     * <ol>
     * <li>Login to Lan Gui using Gateway IP</li>
     * <li>Navigate to wifi page</li>
     * <li>Navigate to wifi 2.4Ghz Configuration page</li>
     * <li>Validate Password Text box is grayed or not</li>
     * <li>Select security mode to Open mode</li>
     * <li>Apply settings and cancel pop up</li>
     * <li>Validate Password Text box is grayed or not</li>
     * <li>Navigate to wifi 5Ghz Configuration page</li>
     * <li>Validate Password Text box is grayed or not</li>
     * <li>Select security mode to Open mode</li>
     * <li>Apply settings and cancel pop up</li>
     * <li>Validate Password Text box is grayed or not</li>
     * </ol>
     * 
     * @param device
     * 
     * @author prasanthreddy.a
     * @refactor Govardhan
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.WIFI)
    @TestDetails(testUID = "TC-RDKB-WIFI-SCRTY-CHECK-1003")
    public void testToVerifyOpnScrtyIn5GhzClnt(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WIFI-SCRTY-CHECK-103";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	Dut lanClient = null;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-SCRTY-CHECK-1003");
	LOGGER.info(
		"TEST DESCRIPTION: Validate Selecting open risky mode and cancelling it immediately should not gray out password field Wifi 5Ghz client");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Login to Lan Gui using Gateway IP");
	LOGGER.info("2. Navigate to wifi page ");
	LOGGER.info("3. Navigate to wifi 2.4Ghz Configuration page");
	LOGGER.info("4. Validate Password Text box is grayed or not");
	LOGGER.info("5. Select security mode to Open mode ");
	LOGGER.info("6. Apply settings and cancel pop up");
	LOGGER.info("7. Validate Password Text box is grayed or not");
	LOGGER.info("8. Navigate to wifi 5Ghz Configuration page");
	LOGGER.info("9. Validate Password Text box is grayed or not");
	LOGGER.info("10. Select security mode to Open mode ");
	LOGGER.info("11. Apply settings and cancel pop up");
	LOGGER.info("12. Validate Password Text box is grayed or not");

	LOGGER.info("#######################################################################################");

	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    lanClient = BroadBandPreConditionUtils.executePreConditionToVerifyWiFiClientStatus(device, tapEnv,
		    BroadBandTestConstants.BAND_5GHZ);
	    LOGGER.info("**********************************************************************************");
	    /**
	     * Step 1 - Step 12 Validates in helper method
	     */
	    helperMethodToOpnScrty(device, lanClient, tapEnv, testCaseId);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-SCRTY-CHECK-1003");
    }

    /**
     * Helper method to validate
     * TC-RDKB-WIFI-SCRTY-CHECK-1001,TC-RDKB-WIFI-SCRTY-CHECK-1002,TC-RDKB-WIFI-SCRTY-CHECK-1003
     * 
     * @param device
     * @param lanClient
     * @param tapEnv
     * @param testCaseId
     * @refactor Govardhan
     */
    public void helperMethodToOpnScrty(Dut device, Dut lanClient, AutomaticsTapApi tapEnv, String testCaseId) {
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	WebDriver webDriver = null;
	try {

	    BroadBandPreConditionUtils.executePreConditionToToggleMeshEnableOrDisableStatus(device, tapEnv,
		    BroadBandTestConstants.BOOLEAN_VALUE_FALSE, BroadBandTestConstants.CONSTANT_1);

	    stepNum = "S1";
	    errorMessage = "Unable to navigate to Gateway page";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Login to Lan Gui using Gateway IP");
	    LOGGER.info("STEP 1: ACTION : Navigate to Gateway ip Res: 10.0.0.1/ Bus : 10.1.10.1");
	    LOGGER.info("STEP 1: EXPECTED : Successfully Navigated to Lan Gui using gateway IP");
	    LOGGER.info("**********************************************************************************");

	    status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, lanClient);
	    webDriver = LanWebGuiLoginPage.getDriver();
	    LanSidePageNavigation lanSidePageNavigation = new LanSidePageNavigation(webDriver);

	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL :Launch Broad band LAN UI login page and verify login status is successful");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S2";
	    errorMessage = "Unable to navigate to wifi configuration page";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Navigate to wifi page ");
	    LOGGER.info(
		    "STEP 2: ACTION : Navigate to Connection - > Wifi Page or <GateWay>/wireless_network_configuration.php");
	    LOGGER.info("STEP 2: EXPECTED : Successfully navigated to wifi configuration page");
	    LOGGER.info("**********************************************************************************");

	    status = lanSidePageNavigation.navigateToWiFiConfigurationPage(device, tapEnv, webDriver);

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Successfully navigated to wifi configuration page");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);
	    stepNum = "S3";
	    errorMessage = "Unable to navigate to wifi 2.4Ghz edit page";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Navigate to wifi 2.4Ghz Configuration page");
	    LOGGER.info("STEP 3: ACTION : Navigate to <GateWay>/wireless_network_configuration_edit.php?id=1");
	    LOGGER.info("STEP 3: EXPECTED : Successfully navigated to wifi 2.4Ghz wifi Page");
	    LOGGER.info("**********************************************************************************");

	    status = lanSidePageNavigation.navigateToPrivateWiFiEditPage(device, tapEnv, webDriver,
		    BroadBandTestConstants.BAND_2_4GHZ);

	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Successfully navigated wifi 2.4Ghz edit page");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S4";
	    errorMessage = "Unable to validate password text box";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Validate Password Text box is grayed or not");
	    LOGGER.info("STEP 4: ACTION : Password text box check whether is not grayed out");
	    LOGGER.info("STEP 4: EXPECTED : Successfully validated password box is not grayed out");
	    LOGGER.info("**********************************************************************************");
	    try {
		status = webDriver.findElement(By.id(BroadBandWebGuiElements.ELEMENT_ID_WIFI_EDIT_PAGE_SSID_PASSWORD))
			.isEnabled();
	    } catch (Exception e) {
		LOGGER.error("Exception caught while validating password text box :" + e.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Successfully verified password text box not grayed out");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("*********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, false);

	    stepNum = "S5";
	    errorMessage = "Unable to select open security mode";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Select security mode to Open mode ");
	    LOGGER.info("STEP 5: ACTION : Click dropdown ->Click more options -> Select open mode");
	    LOGGER.info("STEP 5: EXPECTED : Successfully selected open mode");
	    LOGGER.info("**********************************************************************************");
	    try {
		Select select = new Select(
			webDriver.findElement(By.id(BroadBandWebGuiElements.ELEMENT_ID_SECURITY_MODE)));
		select.selectByValue(BroadBandWebGuiElements.LINK_TEXT_FOR_SUMMARY_MORE);

		webDriver.findElement(By.id(BroadBandWebGuiElements.ELEMENT_ID_OPEN_SECURITY_OPTION)).click();
		status = true;
	    } catch (Exception e) {
		LOGGER.error("Exception caught while selecting security mode :" + e.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Successfully selected security mode open in pop up");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, false);

	    stepNum = "S6";
	    errorMessage = "Unable to cancel settings";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Apply settings and cancel pop up");
	    LOGGER.info("STEP 6: ACTION : Click Apply settings and cancel pop up");
	    LOGGER.info("STEP 6: EXPECTED : Successfully cancelled settings");
	    LOGGER.info("**********************************************************************************");
	    try {
		webDriver.findElement(By.id(BroadBandWebGuiElements.ELEMENT_ID_OK_BUTTON_POP_OK_MESSAGE)).click();
		tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		webDriver.findElement(By.id(BroadBandWebGuiElements.ELEMENT_ID_FOR_CANCEL_IN_POPUP_MESSAGE)).click();
		status = true;
	    } catch (Exception e) {
		LOGGER.error("Exection caught while cancel settings " + e.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Successfully selected apply settings and clicked cancel ");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, false);

	    stepNum = "S7";
	    errorMessage = "Unable to validate password text box";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Validate Password Text box is grayed or not");
	    LOGGER.info("STEP 7: ACTION : Password text box check whether is not grayed out");
	    LOGGER.info("STEP 7: EXPECTED : Successfully validated password box is not grayed out");
	    LOGGER.info("**********************************************************************************");
	    try {
		status = webDriver.findElement(By.id(BroadBandWebGuiElements.ELEMENT_ID_WIFI_EDIT_PAGE_SSID_PASSWORD))
			.isEnabled();
	    } catch (Exception e) {
		LOGGER.error("Exection caught while validating password text box" + e.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Successfully verified password text box not grayed out");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, false);

	    stepNum = "S8";
	    errorMessage = "Unable to navigate to wifi 5Ghz edit page";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Navigate to wifi 5Ghz Configuration page");
	    LOGGER.info("STEP 8: ACTION : Navigate to <GateWay>/wireless_network_configuration_edit.php?id=1");
	    LOGGER.info("STEP 8: EXPECTED : Successfully navigated to wifi 5Ghz wifi Page");
	    LOGGER.info("**********************************************************************************");
	    status = lanSidePageNavigation.navigateToWiFiConfigurationPage(device, tapEnv, webDriver);
	    if (status) {
		status = lanSidePageNavigation.navigateToPrivateWiFiEditPage(device, tapEnv, webDriver,
			BroadBandTestConstants.BAND_5GHZ);
	    }
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL :  Successfully navigated wifi 5Ghz edit page");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, false);

	    stepNum = "S9";
	    errorMessage = "Unable to validate password text box";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION : Validate Password Text box is grayed or not");
	    LOGGER.info("STEP 9: ACTION : Password text box check whether is not grayed out");
	    LOGGER.info("STEP 9: EXPECTED : Successfully validated password box is not grayed out");
	    LOGGER.info("**********************************************************************************");
	    try {
		status = webDriver.findElement(By.id(BroadBandWebGuiElements.ELEMENT_ID_WIFI_EDIT_PAGE_SSID_PASSWORD))
			.isEnabled();
	    } catch (Exception e) {
		LOGGER.error("Exection caught while validating password text box" + e.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : Successfully verified password text box not grayed out");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, false);

	    stepNum = "S10";
	    errorMessage = "Unable to select open security mode";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Select security mode to Open mode ");
	    LOGGER.info("STEP 10: ACTION : Click dropdown ->Click more options -> Select open mode");
	    LOGGER.info("STEP 10: EXPECTED : Successfully selected open mode");
	    LOGGER.info("**********************************************************************************");
	    try {
		Select select = new Select(
			webDriver.findElement(By.id(BroadBandWebGuiElements.ELEMENT_ID_SECURITY_MODE)));

		select.selectByValue(BroadBandWebGuiElements.LINK_TEXT_FOR_SUMMARY_MORE);

		webDriver.findElement(By.id(BroadBandWebGuiElements.ELEMENT_ID_OPEN_SECURITY_OPTION)).click();
		status = true;
	    } catch (Exception e) {
		LOGGER.error("Exection caught while selecting security mode :" + e.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : Successfully selected security mode open in pop up");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, false);

	    stepNum = "S11";
	    errorMessage = "Unable to cancel settings";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 11: DESCRIPTION : Apply settings and cancel pop up");
	    LOGGER.info("STEP 11: ACTION : Click Apply settings and cancel pop up");
	    LOGGER.info("STEP 11: EXPECTED : Successfully cancelled settings");
	    LOGGER.info("**********************************************************************************");
	    try {
		webDriver.findElement(By.id(BroadBandWebGuiElements.ELEMENT_ID_OK_BUTTON_POP_OK_MESSAGE)).click();
		tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		webDriver.findElement(By.id(BroadBandWebGuiElements.ELEMENT_ID_FOR_CANCEL_IN_POPUP_MESSAGE)).click();
		status = true;
	    } catch (Exception e) {
		LOGGER.error("Exection caught while cancel settings :" + e.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 11: ACTUAL : Successfully selected apply settings and clicked cancel ");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, false);

	    stepNum = "S12";
	    errorMessage = "Unable to validate password text box";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 12: DESCRIPTION : Validate Password Text box is grayed or not");
	    LOGGER.info("STEP 12: ACTION : Password text box check whether is not grayed out");
	    LOGGER.info("STEP 12: EXPECTED : Successfully validated password box is not grayed out");
	    LOGGER.info("**********************************************************************************");
	    try {
		status = webDriver.findElement(By.id(BroadBandWebGuiElements.ELEMENT_ID_WIFI_EDIT_PAGE_SSID_PASSWORD))
			.isEnabled();
	    } catch (Exception e) {
		LOGGER.error("Exection caught while validating password text box" + e.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 12: ACTUAL : Successfully verified password text box not grayed out");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, false);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {

	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");

	    BroadBandPostConditionUtils.postConditionCloseBrowser(status, stepNumber);

	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
    }
    
    /**
     * Verify that Wifi client gets a valid IPv6 prefix, when the gateway is configured with SLAAC
     * <ol>
     * <li>Launch Broad band WebUI login page and verify login status.</li>
     * <li>Launch Connection page and navigate to "Local IP Network"</li>
     * <li>Verify stateless auto config is enabled and saved by unchecking the stateful check box(enabling stateless
     * auto config)</li>
     * <li>Launch Connection page and navigate to \"<PARTNER> Network\""</li>
     * <li>Verify the delegated Ipv6 prefix in the UI</li>
     * <li>Retrieve the Ipv6 address from the client</li>
     * <li>Verify ipv6 address obtained should contain the valid delegated prefix</li>
     * </ol>
     * 
     * @refactor Said Hisham
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-LAN-GUI-7001")
    public void verifyIpv6PrefixInSLAACModeForWifiClient(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-LAN-GUI-701";
	String stepNum = "S1";
	String errorMessage = "";
	boolean status = false;
	Dut deviceConnected = null;// Dut object to store client
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-LAN-GUI-7001");
	LOGGER.info(
		"TEST DESCRIPTION: Verify that Wifi client gets a valid IPv6 prefix, when the gateway is configured with SLAAC");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Launch Broad band WebUI login page and verify login status.");
	LOGGER.info("2. Launch Connection page and navigate to \"Local IP Network\"");
	LOGGER.info("3. Verify stateless auto config checkbox is enabled and saved ");
	LOGGER.info("4. Launch Connection page and navigate to \"<PARTNER> Network\"");
	LOGGER.info("5. Verify the delegated Ipv6 prefix in the UI");
	LOGGER.info("6.Retrieve the Ipv6 address from the client");
	LOGGER.info("7. Verify ipv6 address obtained should contain the valid delegated prefix");
	LOGGER.info("#######################################################################################");

	try {
	    // calling the precondition for interface and Connectivity
	    deviceConnected = BroadBandPreConditionUtils.executePreConditionToVerifyWiFiClientStatus(device, tapEnv,
		    BroadBandTestConstants.BAND_2_4GHZ);
	    LOGGER.info("##########################################################################");

	    verifyStatelessAutoConfigThroughWebGui(device, testCaseId, deviceConnected);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-LAN-GUI-7001");
    }

    /**
     * Verify that LAN client gets a valid IPv6 prefix, when the gateway is configured with SLAAC
     * <ol>
     * <li>Launch Broad band WebUI login page and verify login status.</li>
     * <li>Launch Connection page and navigate to "Local IP Network"</li>
     * <li>Verify stateless auto config is enabled and saved by unchecking the stateful check box(enabling stateless
     * auto config)</li>
     * <li>Launch Connection page and navigate to "<Partner> Network"</li>
     * <li>Verify the delegated Ipv6 prefix in the UI</li>
     * <li>Retrieve the Ipv6 address from the client</li>
     * <li>Verify ipv6 address obtained should contain the valid delegated prefix</li>
     * </ol>
     * 
     * @refactor Said Hisham
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-LAN-GUI-7002")
    public void verifyIpv6PrefixInSLAACModeForEthernetClient(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-LAN-GUI-702";
	String stepNum = "S1";
	String errorMessage = "";
	boolean status = false;
	Dut deviceConnectedWithEthernet = null;// Dut object to store client
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-LAN-GUI-7002");
	LOGGER.info(
		"TEST DESCRIPTION: Verify that LAN client gets a valid IPv6 prefix, when the gateway is configured with SLAAC");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Launch Broad band WebUI login page and verify login status.");
	LOGGER.info("2. Launch Connection page and navigate to \"Local IP Network\"");
	LOGGER.info("3. Verify stateless auto config checkbox is enabled and saved ");
	LOGGER.info("4. Launch Connection page and navigate to \"<Partner> Network\"");
	LOGGER.info("5. Verify the delegated Ipv6 prefix in the UI");
	LOGGER.info("6.Retrieve the Ipv6 address from the client");
	LOGGER.info("7. Verify ipv6 address obtained should contain the valid delegated prefix");
	LOGGER.info("#######################################################################################");

	try {
	    // calling the precondition for interface and Connectivity
	    deviceConnectedWithEthernet = BroadBandPreConditionUtils.executePreConditionToVerifyLanClientStatus(device,
		    tapEnv);
	    LOGGER.info("##########################################################################");

	    verifyStatelessAutoConfigThroughWebGui(device, testCaseId, deviceConnectedWithEthernet);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-LAN-GUI-7002");
    }
    
    /**
     * Method to verify Ipv6 address as delegated prefix when stateless auto config is enabled
     * 
     * @param device
     *            {@link Dut}
     * @param testCaseId
     *            Instance for Test Case ID.
     * @param deviceConnected
     *            Instance for Device Connected.
     * @refactor Said Hisham
     */
    public void verifyStatelessAutoConfigThroughWebGui(Dut device, String testCaseId, Dut deviceConnected) {
	// Variable Declaration begins
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	LanSidePageNavigation lanSidePageNavigation = null;
	String delegatedPrefixRetrieved = null;
	String ipv6AddressRetrievedFromClient = null;
	boolean isStatelessConfigured = false;
	WebDriver webDriver = null;
	// Variable Declaration Ends
	try {
	    stepNum = "S1";
	    errorMessage = "Unable to Login to LanGUI page using Admin credential";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Launch Broad band LAN UI login page and verify login status");
	    LOGGER.info(
		    "STEP 1: ACTION : Launch the below URL format in browser <LAN PAGE URL> /LOGIN CREDENTIALS :  username: <USERNAME> Password: <PASSWORD>");
	    LOGGER.info("STEP 1: EXPECTED : page should be launched and login should be successful");
	    LOGGER.info("**********************************************************************************");
	    status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, deviceConnected);
	    webDriver = LanWebGuiLoginPage.getDriver();
	    lanSidePageNavigation = new LanSidePageNavigation(webDriver);
	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : Launch Broad band LAN UI login page and verify login status is successful");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S2";
	    errorMessage = "Unable to verify navigation status on connection  page";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Launch Connection page and navigate to \"Local IP Network\"");
	    LOGGER.info(
		    "STEP 2: ACTION : Launch Connection page from GUI \"Gateway>connection \"and from Connection page navigate to Wifi page \"connection>Local IP Network\"");
	    LOGGER.info(
		    "STEP 2: EXPECTED : Connection page launch and navigate from Connection page to Local IP Network should be successful");
	    LOGGER.info("**********************************************************************************");
	    status = lanSidePageNavigation.navigateToLocalIpConfigurationPage(device, tapEnv, webDriver);
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Successfully navigated to Local IP Network Page");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S3";
	    errorMessage = "unchecking the stateful check-box in UI page is not successful";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Verify stateless auto config is enabled and saved by unchecking the stateful check box(enabling stateless auto config)");
	    LOGGER.info("STEP 3: ACTION : stateful Check box should be disabled");
	    LOGGER.info(
		    "STEP 3: EXPECTED : stateful Checkbox should be unchecked and stateless auto config should be saved");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebUiUtils.selectSLAACMode(tapEnv, webDriver, true);
	    if (status) {
		isStatelessConfigured = true;
		LOGGER.info("STEP 3: ACTUAL : Successfully unchecked the  Stateful check box");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S4";
	    errorMessage = "Unable to navigate to <PARTNER> network page";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Launch Connection page and navigate to \"<PARTNER> Network\"");
	    LOGGER.info("STEP 4: ACTION : Navigate to <PARTNER> network page Gateway->Connection -><PARTNER> Network");
	    LOGGER.info("STEP 4: EXPECTED : Navigation should be successful");
	    LOGGER.info("**********************************************************************************");
	    boolean isBusinessDevice = DeviceModeHandler.isBusinessClassDevice(device);
	    status = lanSidePageNavigation.navigateToPartnerNetworkPage(device, tapEnv, webDriver, isBusinessDevice);

	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Successfully navigate to <PARTNER> Network page");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S5";
	    errorMessage = "Unable to retrieve the valid Delegated Prefix Address (IPv6) from <PARTNER> Network page";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify the Delegated prefix (IPv6)");
	    LOGGER.info("STEP 5: ACTION : Retrive the Delegated prefix (IPv6) using the Xpath element.");
	    LOGGER.info("STEP 5: EXPECTED : Retrieving the Delegated prefix (IPv6) should be successful.");
	    LOGGER.info("**********************************************************************************");
	    try {
		if (isBusinessDevice) {
		    delegatedPrefixRetrieved = webDriver
			    .findElement(By.xpath(BroadBandWebGuiElements.XPATH_FOR_DELEGATED_PREFIX_IPV6_BUSINESS))
			    .getText();
		} else {
		    delegatedPrefixRetrieved = webDriver
			    .findElement(By.xpath(BroadBandWebGuiElements.XPATH_FOR_DELEGATED_PREFIX_IPV6)).getText();
		}
		if (CommonMethods.isNotNull(delegatedPrefixRetrieved)) {
		    delegatedPrefixRetrieved = delegatedPrefixRetrieved.trim().split("/")[0];
		    LOGGER.info("Delegated prefix (IPv6) retrieved from Web GUI : " + delegatedPrefixRetrieved);
		    status = CommonMethods.isIpv6Address(delegatedPrefixRetrieved);
		}
	    } catch (Exception e) {
		status = false;
		LOGGER.error(" Exception occured while getting Delegated prefix (IPv6) from <PARTNER> Network page"
			+ e.getMessage());
	    }
	    if (status) {
		LOGGER.info(
			"STEP 5: ACTUAL :  Successfully retrieved a valid IPV6 adrress for Delegated Prefix from GUI");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S6";
	    errorMessage = "Unable to retrieve the Global Ipv6 address from the client";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Retrieve the Ipv6 address from the client");
	    LOGGER.info("STEP 6: ACTION : Get the Ipv6 address from the client connected");
	    LOGGER.info("STEP 6: EXPECTED : Ipv6 Address must be retrieved from the client ");
	    LOGGER.info("**********************************************************************************");
	    Device ecatsSettop = (Device) deviceConnected;
	    String connectionType = ecatsSettop.getConnectedDeviceInfo().getConnectionType();
	    LOGGER.info("connection type :" + connectionType);
	    if (CommonMethods.isNotNull(connectionType)
		    && BroadBandWebGuiTestConstant.CONNECTION_TYPE_WIFI.equalsIgnoreCase(connectionType)) {
		ipv6AddressRetrievedFromClient = BroadBandConnectedClientUtils
			.retrieveIPv6AddressFromConnectedClientWithDeviceCOnnected(deviceConnected, tapEnv);
		LOGGER.info("ipv6 address retrieved from client if connection type is wifi :"
			+ ipv6AddressRetrievedFromClient);
	    }
	    if (CommonMethods.isNotNull(connectionType)
		    && BroadBandWebGuiTestConstant.CONNECTION_TYPE_ETHERNET.equalsIgnoreCase(connectionType)) {
		ipv6AddressRetrievedFromClient = BroadBandConnectedClientUtils
			.retrieveIPv6AddressFromConnectedClientWithDeviceCOnnected(deviceConnected,
				tapEnv);
		LOGGER.info("ipv6 address retrieved from client if connection type is ethernet :"
			+ ipv6AddressRetrievedFromClient);
	    }
	    status = CommonMethods.isNotNull(ipv6AddressRetrievedFromClient)
		    && CommonMethods.isIpv6Address(ipv6AddressRetrievedFromClient);
	    LOGGER.info("Ipv6AddressRetrievedFromClient-" + ipv6AddressRetrievedFromClient);
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Retrieval of Ipv6 address from the client is successful");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S7";
	    errorMessage = "Ipv6  prefix is not as same as delegated prefix";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify ipv6 address obtained should contain the valid delegated prefix");
	    LOGGER.info(
		    "STEP 7: ACTION : Verifty the Ipv6 address from the below command \" EXECUTE COMMAND, WINDOWS : ipconfig |grep -A 10 \"Wireless adapter Wi-Fi\" |grep -i \"IPv6 Address\" or LINUX : ifconfig | grep \"inet6\" or ON THE CONNECTED CLIENT.\" and should be same as the retrieved delegated prefix ipv6 address");
	    LOGGER.info(
		    "STEP 7: EXPECTED : The LAN client generates its own IPv6 address  and should be same as the   delegated prefix ipv6 address  from the gateway . The prefix must be a /64 one");
	    LOGGER.info("**********************************************************************************");

	    status = CommonUtils.patternSearchFromTargetString(
		    ipv6AddressRetrievedFromClient.replace(BroadBandTestConstants.DELIMITER_COLON,
			    BroadBandTestConstants.EMPTY_STRING),
		    delegatedPrefixRetrieved.replace(BroadBandTestConstants.DELIMITER_COLON,
			    BroadBandTestConstants.EMPTY_STRING));
	    if (status) {
		LOGGER.info(
			"STEP 7: ACTUAL : Client generates its own IPv6 address  and should be same as the   delegated prefix ipv6 address  from the gateway");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info("POST-CONDITION : DESCRIPTION : Enabling the stateful configuration");
	    LOGGER.info("POST-CONDITION : ACTION : Enable the stateful configuration check box and click save button");
	    LOGGER.info("POST-CONDITION : EXPECTED : Stateful configuration should be saved sucessfully");
	    boolean navigationStatus = false;
	    if (isStatelessConfigured) {
		navigationStatus = lanSidePageNavigation.navigateToLocalIpConfigurationPage(device, tapEnv, webDriver);
		LOGGER.info("navigationStatus-" + navigationStatus);
		if (navigationStatus) {
		    status = BroadBandWebUiUtils.selectSLAACMode(tapEnv, webDriver, false);
		} else {
		    LOGGER.info("Navigation to local Ip configuration page failed");
		}
	    }
	    if (status) {
		LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
	    } else {
		LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
    }
    
    /**
     * Verify DHCPv6 address in 2.4Ghz Wifi connected client on changing Min Address and Max Address
     * <ol>
     * <li>Login to Lan side GUI page of device Using URL: Business Class Device:http://10.1.10.1 Residential Class
     * Device:http://10.0.0.1</li>
     * <li>Launch Connection page and navigate to \"Connection status\"</li>
     * <li>Verify DHCPv6 Lease Time element is removed from status page</li>
     * <li>Launch Connection page and navigate to \"Local IP Network\"</li>
     * <li>Verify DHCPv6 Lease Time element is removed from status page</li>
     * <li>Change DHCPv6 Beginning Address in UI page</li>
     * <li>Change DHCPv6 Ending Address in UI page</li>
     * <li>Verify DHCPv6 Beginning,ending and lease time should be verified using webpa</li>
     * <li>Verify whether Wifi client IPv6 Address is in DHCPv6 range</li>
     * <li>Verify internet is accessibble by using intetface IPv6 on the Wifi client</li>
     * <li>Disconnect WiFi from the client</li>
     * <li>INITIATE A PACKET CAPTURE ON THE DEVICE</li>
     * <li>Connect client to 5Ghz SSID WiFi</li>
     * <li>GENERATE TRAFFIC BY PING OR CURL ON CONNECTED DEVICE</li>
     * <li>KILL AND VERIFY THE TCP DUMP ON THE DEVICE</li>
     * <li>Verify Preferred and valid lifetime from DHCP packets is same with TR181 parameters</li>
     * </ol>
     * 
     * @author Betel Costrow
     * @refactor Said Hisham
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-DHCP-IPV6-1002")
    public void testToVerifyDhcpv6AddressIn24GhzWifiClient(Dut device) {

	// Variable Declaration begins
	String testCaseId = "";
	String errorMessage = null;
	boolean status = false;
	Dut wifiConnectedClient = null;
	boolean isBrowserOpen = false;
	Map<String, String> defaultDhcpWebpaOutput = null;
	String[] arrayOfDhcpParams = { BroadBandWebPaConstants.WEB_PARAM_DELEGATED_PREFIX_IPV6,
		BroadBandWebPaConstants.WEB_PARAM_DHCPV6_BEGINNING_ADDRESS,
		BroadBandWebPaConstants.WEB_PARAM_DHCPV6_ENDING_ADDRESS };

	BroadBandResultObject broadBandResultObject = new BroadBandResultObject();
	String preferredLifetime = null;
	String validLifetime = null;
	// Variable Declaration Ends

	testCaseId = "TC-RDKB-DHCP-IPV6-102";

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-DHCP-IPV6-1002");
	LOGGER.info(
		"TEST DESCRIPTION: Verify DHCPv6 address  in 2.4Ghz Wifi connected client  on changing Min Address and Max Address ");
	LOGGER.info("PRE-CONDITION STEPS");
	LOGGER.info("PRE-CONDITION 1 : OBTAIN A 2.4GHZ WIFI CLIENT ASSOSIATED WITH THE GATEWAY");
	LOGGER.info("PRE-CONDITION 2 : RETRIEVE DHCPV6 BEGINNING ,ENDING ADDRESS USING WEBPA");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info(
		"1. Login to Lan side GUI page of device Using URL:Business Class Device:http://10.1.10.1 Residential Class Device:http://10.0.0.1");
	LOGGER.info("2. Launch Connection page and navigate to \"Connection status\"");
	LOGGER.info("3. Verify DHCPv6 Lease Time element is removed from status page");
	LOGGER.info("4. Launch Connection page and navigate to \"Local IP Network\"");
	LOGGER.info("5. Verify DHCPv6 Lease Time element is removed from status page");
	LOGGER.info("6. Change DHCPv6 Beginning Address in UI page");
	LOGGER.info("7. Change DHCPv6 Ending Address in UI page");
	LOGGER.info("8. Verfiy DHCPv6 Beginning,ending Address should be verified using webpa");
	LOGGER.info("9. Verify whether Wifi client IPv6 Address is in DHCPv6 range");
	LOGGER.info("10. Verify internet is accessibble by using intetface IPv6 on the Wifi client");
	LOGGER.info("11. Disconnect WiFi from the client");
	LOGGER.info("12. INITIATE A PACKET CAPTURE ON THE DEVICE");
	LOGGER.info("13. Connect client to 5Ghz SSID WiFi");
	LOGGER.info("14. GENERATE TRAFFIC BY PING OR CURL ON CONNECTED DEVICE");
	LOGGER.info("15. KILL AND VERIFY THE TCP DUMP ON THE DEVICE");
	LOGGER.info("16. Verify Preferred and valid lifetime from DHCP packets is same with TR181 parameters");

	LOGGER.info("#######################################################################################");
	try {
	    try {
		LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
		LOGGER.info("PRE-CONDITION STEPS");

		wifiConnectedClient = BroadBandPreConditionUtils.executePreConditionToVerifyWiFiClientStatus(device,
			tapEnv, BroadBandTestConstants.BAND_2_4GHZ, BroadBandTestConstants.CONSTANT_1);

		LOGGER.info("######################################################################################");
		LOGGER.info(
			"PRE-CONDITION 2 : DESCRIPTION : Retrieve DHCPV6 Beginning ,Ending and Lease time using Webpa");
		LOGGER.info("PRE-CONDITION 2 : ACTION : Execute webpa command to retrieve values");
		LOGGER.info("PRE-CONDITION 2 : EXPECTED : Webpa command should be executed successfully");
		LOGGER.info("######################################################################################");
		defaultDhcpWebpaOutput = tapEnv.executeMultipleWebPaGetCommands(device, arrayOfDhcpParams);
		broadBandResultObject = BroadBandWebPaUtils.verifyDhcpv6OutputValues(defaultDhcpWebpaOutput);
		status = broadBandResultObject.isStatus();
		errorMessage = broadBandResultObject.getErrorMessage();
		if (status) {
		    LOGGER.info(
			    "PRE-CONDITION 2: ACTUAL : DHCPV6 Beginning ,Ending and Lease time retrieved successfully.");
		} else {
		    LOGGER.error(
			    "PRE-CONDITION 2: ACTUAL : Unable to retrieve DHCPV6 Beginning ,Ending and Lease time.");
		    throw new TestException(
			    BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 2 : FAILED : " + errorMessage);
		}
		LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
		LOGGER.info("**********************************************************************************");
	    } catch (Exception e) {
		errorMessage = "Exception occured in Pre-Condition while verifying change in DHCP address in Ethernet client."
			+ e.getMessage();
		LOGGER.error(errorMessage);
		CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
			true);
	    }
	    
	    changeValuesOfDhcpv6InLanSideUI(device, wifiConnectedClient, tapEnv, testCaseId, isBrowserOpen,
		    BroadBandTestConstants.BAND_2_4GHZ);

	    stepNum = "S11";
	    status = false;
	    errorMessage = null;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP 11 : DESCRIPTION : Disconnect WiFi from the client");
	    LOGGER.info("STEP 11 : ACTION : Get connected Wifi SSID & passphrase and disconnect it");
	    LOGGER.info("STEP 11 : EXPECTED : WiFi must be disconnected in the client");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Unable to disconnect the WiFi in the client " + broadBandResultObject.getErrorMessage();
	    broadBandResultObject = BroadBandConnectedClientUtils.disconnectCnnClientFromSsid(tapEnv, device,
		    wifiConnectedClient);
	    status = broadBandResultObject.isStatus();
	    if (status) {
		LOGGER.info("STEP 11 : ACTUAL : Successfully disconnected WiFi in the client");
	    } else {
		LOGGER.error("STEP 11 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S12";
	    status = false;
	    errorMessage = null;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP 12 : DESCRIPTION : INITIATE A PACKET CAPTURE ON THE DEVICE");
	    LOGGER.info("STEP 12 : ACTION : EXECUTE COMMAND : tcpdump -i brlan0 -w /tmp/dhcp.pcap");
	    LOGGER.info("STEP 12 : EXPECTED : PACKET CAPTURE INITIALIZE MUST BE SUCCESSFUL");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "PACKET CAPTURE INITIALIZE FAILED";
	    String response = null;
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		LOGGER.info("Downloading tcpdump from auto vault for atom sync device.");
		CommonUtils.downloadFileUsingAutoVault(device, tapEnv, BroadBandCommandConstants.FILE_PATH_TCPDUMP,
			BroadBandCommandConstants.FILE_PATH_RUN);
		tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_PERMISSION_TO_TCPDUMP_RUN);
		response = tapEnv.executeCommandUsingSsh(device,
			BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.FILE_PATH_RUN,
				BroadBandCommandConstants.PROCESS_NAME_TCPDUMP,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandCommandConstants.CMD_PACKET_CAPTURE_BRLAN0_DHCP,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandCommandConstants.FILE_PATH_DHCP_PACKET_CAPTURE));
	    } else {
		response = tapEnv.executeCommandUsingSsh(device,
			BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.FILE_PATH_USR_BIN,
				BroadBandCommandConstants.PROCESS_NAME_TCPDUMP,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandCommandConstants.CMD_PACKET_CAPTURE_BRLAN0_DHCP,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandCommandConstants.FILE_PATH_DHCP_PACKET_CAPTURE));
	    }
	    LOGGER.info("Packet Capture Response : " + response);
	    status = CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
		    BroadBandTraceConstants.LOG_MESSAGE_FOR_BRLAN_PACKET_CAPTURE_INIT);
	    if (status) {
		LOGGER.info("STEP 12 : ACTUAL : PACKET CAPTURE INITIALIZATION IS SUCCESSFUL");
	    } else {
		LOGGER.error("STEP 12 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S13";
	    status = false;
	    errorMessage = null;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP 13 : DESCRIPTION : Connect client to 5Ghz SSID WiFi");
	    LOGGER.info("STEP 13 : ACTION : Get 5Ghz SSID & passphrase and connect client to it");
	    LOGGER.info("STEP 13 : EXPECTED : client must be connected to 5Ghz SSID WiFi");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "Unable to connect 5Ghz SSID WiFi in the client";
	    LOGGER.info("Connecting WiFi to the 5Ghz SSID to get the DHCP renewed ip from packets.");
	    broadBandResultObject = BroadBandConnectedClientUtils.connectGivenConnectedClientToWifi(device, tapEnv,
		    wifiConnectedClient, WiFiFrequencyBand.WIFI_BAND_5_GHZ);
	    status = broadBandResultObject.isStatus();
	    if (status) {
		LOGGER.info("STEP 13 : ACTUAL : Successfully connected client to 5Ghz SSID WiFi");
	    } else {
		LOGGER.error("STEP 13 : ACTUAL : " + errorMessage + broadBandResultObject.getErrorMessage());
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S14";
	    status = false;
	    errorMessage = null;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP 14 : DESCRIPTION : GENERATE TRAFFIC BY PING OR CURL ON CONNECTED DEVICE");
	    LOGGER.info(
		    "STEP 14 : ACTION : EXECUTE COMMAND : Ping or curl on following URL's www.instagram.com,www.ebay.com,www.w3schools.com,www.wikipedia.org ON THE CONNECTED CLIENT ");
	    LOGGER.info("STEP 14 : EXPECTED : TRAFFIC GENERATION MUST BE SUCCESSFUL ON CONNECTED DEVICE");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "TRAFFIC GENERATION FAILED IN CONNECTED DEVICE AND INTERNET IS NOT ACCESSIBLE IN CLIENT";
	    status = BroadBandWiFiUtils.generateTrafficUsingCurlOrPing(device, wifiConnectedClient);
	    if (status) {
		LOGGER.info("STEP 14 : ACTUAL : TRAFFIC GENERATION IS SUCCESSFUL");
	    } else {
		LOGGER.error("STEP 14 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S15";
	    status = false;
	    errorMessage = null;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP 15 : DESCRIPTION : KILL AND VERIFY THE TCP DUMP ON THE DEVICE");
	    LOGGER.info("STEP 15 : ACTION : EXECUTE COMMAND : killall tcpdump");
	    LOGGER.info("STEP 15 : EXPECTED : KILL TCP DUMP MUST BE SUCCESSFUL PACKET CAPTURE PROCESS ");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "UNABLE TO KILL THE TCP DUMP PROCESS";
	    status = BroadBandCommonUtils.killAndVerifyProcess(device, tapEnv,
		    BroadBandCommandConstants.PROCESS_NAME_TCPDUMP);
	    if (status) {
		LOGGER.info("STEP 15 : ACTUAL : TCP DUMP PROCESS KILLED SUCCESSFULLY");
	    } else {
		LOGGER.error("STEP 15 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S16";
	    status = false;
	    errorMessage = null;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info(
		    "STEP 16 : DESCRIPTION : Verify Preferred and valid lifetime from DHCP packets is same with TR181 parameters");
	    LOGGER.info(
		    "STEP 16 : ACTION : EXECUTE COMMAND : 1) tcpdump -qns 0 -A -r /tmp/dhcp.pcap | grep -i 'dhcpv6' "
			    + "2) Webpa get Device.IP.Interface.1.IPv6Prefix.1.X_CISCO_COM_PreferredLifetime "
			    + "3)Webpa get Device.IP.Interface.1.IPv6Prefix.1.X_CISCO_COM_ValidLifetime "
			    + "4) Compare Packet capture and TR181 have same value for preferred and valid lifetime");
	    LOGGER.info(
		    "STEP 16 : EXPECTED : DHCP Packets and TR181 should have same value for preferred and valid lifetime");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "DHCPv6 packets are not received in /tmp/dhcp.pcap";
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
			BroadBandCommandConstants.FILE_PATH_RUN, BroadBandCommandConstants.PROCESS_NAME_TCPDUMP,
			BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			BroadBandCommandConstants.CMD_TO_READ_PACKET_CAPTURE,
			BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			BroadBandCommandConstants.FILE_PATH_DHCP_PACKET_CAPTURE, BroadBandTestConstants.SYMBOL_PIPE,
			BroadBandTestConstants.GREP_COMMAND, BroadBandTestConstants.DOUBLE_QUOTE,
			BroadBandTestConstants.PROTOCOL_DHCPV6, BroadBandTestConstants.DOUBLE_QUOTE));
	    } else {
		response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
			BroadBandCommandConstants.FILE_PATH_USR_BIN, BroadBandCommandConstants.PROCESS_NAME_TCPDUMP,
			BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			BroadBandCommandConstants.CMD_TO_READ_PACKET_CAPTURE,
			BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			BroadBandCommandConstants.FILE_PATH_DHCP_PACKET_CAPTURE, BroadBandTestConstants.SYMBOL_PIPE,
			BroadBandTestConstants.GREP_COMMAND, BroadBandTestConstants.DOUBLE_QUOTE,
			BroadBandTestConstants.PROTOCOL_DHCPV6, BroadBandTestConstants.DOUBLE_QUOTE));
	    }
	    LOGGER.info("Response of dhcpv6 grep is : " + response);
	    if (CommonMethods.isNotNull(response)) {
		errorMessage = "Failed to get values of " + BroadBandWebPaConstants.WEBPA_PARAM_PREFERRED_LIFETIME
			+ " and " + BroadBandWebPaConstants.WEBPA_PARAM_VALID_LIFETIME;
		preferredLifetime = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_PREFERRED_LIFETIME);
		validLifetime = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_VALID_LIFETIME);
		if (CommonMethods.isNotNull(preferredLifetime) && CommonMethods.isNotNull(validLifetime)) {
		    errorMessage = "DHCP Packets and TR181 not having same value for preferred and valid lifetime";
		    preferredLifetime = BroadBandCommonUtils.concatStringUsingStringBuffer(
			    BroadBandTestConstants.STRING_PREFERRED_LIFETIME, preferredLifetime);
		    validLifetime = BroadBandCommonUtils
			    .concatStringUsingStringBuffer(BroadBandTestConstants.STRING_VALID_LIFETIME, validLifetime);
		    status = CommonMethods.isGivenStringAvailableInCommandOutput(response, preferredLifetime)
			    && CommonMethods.isGivenStringAvailableInCommandOutput(response, validLifetime);
		}
	    }

	    if (status) {
		LOGGER.info(
			"STEP 16 : ACTUAL : Preferred and valid lifetime from DHCP packets is same with TR181 parameters");
	    } else {
		LOGGER.error("STEP 16 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    // Post condition to revert back changes
	    postconditionTorevertDhcpChanges(device, tapEnv, defaultDhcpWebpaOutput, BroadBandTestConstants.CONSTANT_1);
	    postConditionCloseBrowser(isBrowserOpen, BroadBandTestConstants.CONSTANT_2);
	    postConditionDisconnectSsid(device, wifiConnectedClient, tapEnv, BroadBandTestConstants.CONSTANT_3);

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("POST-CONDITION 4 : DESCRIPTION : Kill tcpdump process running as background process");
	    LOGGER.info("POST-CONDITION 4 : ACTION : killall -11 tcpdump");
	    LOGGER.info("POST-CONDITION 4 : EXPECTED : tcpdump process must be killed");
	    LOGGER.info("#######################################################################################");
	    status = BroadBandCommonUtils.killAndCheckProcess(device, tapEnv, BroadBandCommandConstants.CMD_TCPDUMP);

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("POST-CONDITION 5 : DESCRIPTION : Remove dhcp.pcap from tmp folder");
	    LOGGER.info("POST-CONDITION 5 : ACTION : rm /tmp/dhcp.pcap");
	    LOGGER.info("POST-CONDITION 5 : EXPECTED : /tmp/dhcp.pcap must be removed");
	    LOGGER.info("#######################################################################################");
	    status = BroadBandCommonUtils.removeFileAndVerifyStatus(tapEnv, device,
		    BroadBandCommandConstants.FILE_PATH_DHCP_PACKET_CAPTURE);

	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		LOGGER.info("#######################################################################################");
		LOGGER.info("POST-CONDITION 6 : DESCRIPTION : Remove tcpdump from run folder");
		LOGGER.info("POST-CONDITION 6 : ACTION : rm /run/tcpdump");
		LOGGER.info("POST-CONDITION 6 : EXPECTED : /run/tcpdump must be removed");
		LOGGER.info("#######################################################################################");
		status = BroadBandCommonUtils.removeFileAndVerifyStatus(tapEnv, device,
			BroadBandCommandConstants.FILE_PATH_RUN + BroadBandCommandConstants.PROCESS_NAME_TCPDUMP);
	    }

	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");

	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-DHCP-IPV6-1002");
    }

    /**
     * Method postcondition to disconnect wifi
     * 
     * @param device
     * @param connectedClient
     * @param tapApi
     * @param stepNumber
     * @refactor Said Hisham
     */
    public void postConditionDisconnectSsid(Dut device, Dut connectedClient, AutomaticsTapApi tapApi,
	    Integer stepNumber) {
	LOGGER.info("#######################################################################################");
	LOGGER.info("POST-CONDITION " + stepNumber + " : DESCRIPTION : Disconnect Wifi Radio SSID ");
	LOGGER.info("POST-CONDITION " + stepNumber + " : ACTION :Disconnect wifi radio SSID in connected client");
	LOGGER.info(
		"POST-CONDITION " + stepNumber + " : EXPECTED : Wifi radio SSID should be disconnected successfully");
	LOGGER.info("#######################################################################################");
	BroadBandResultObject broadBandResultObject = new BroadBandResultObject();
	if (connectedClient != null) {
	    broadBandResultObject = BroadBandConnectedClientUtils.disconnectCnnClientFromSsid(tapApi, device,
		    connectedClient);
	}
	LOGGER.info("POST CONDITION " + stepNumber + ":ACTUAL: WIFI SSID 2.4GHZ Disconnect status:"
		+ broadBandResultObject.isStatus());
	LOGGER.info("#######################################################################################");
    }

    /**
     * Verify DHCPv6 address in 5Ghz Wifi connected client on changing Min Address and Max Address
     * <ol>
     * <li>Login to Lan side GUI page of device Using URL: Business Class Device:http://10.1.10.1 Residential Class
     * Device:http://10.0.0.1</li>
     * <li>Launch Connection page and navigate to \"Connection status\"</li>
     * <li>Verify DHCPv6 Lease Time element is removed from status page</li>
     * <li>Launch Connection page and navigate to \"Local IP Network\"</li>
     * <li>Verify DHCPv6 Lease Time element is removed from status page</li>
     * <li>Change DHCPv6 Beginning Address in UI page</li>
     * <li>Change DHCPv6 Ending Address in UI page</li>
     * <li>Verify DHCPv6 Beginning,ending and lease time should be verified using webpa</li>
     * <li>Verify whether Wifi client IPv6 Address is in DHCPv6 range</li>
     * <li>Verify internet is accessible by using interface IPv6 on the Wifi client</li>
     * </ol>
     * 
     * @refactor Said Hisham
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-DHCP-IPV6-1003")
    public void testToVerifyDhcpv6AddressIn5GhzWifiClient(Dut device) {

	// Variable Declaration begins
	String testCaseId = "";
	String errorMessage = "";
	boolean status = false;
	Dut wifiConnectedClient = null;

	boolean isBrowserOpen = false;
	Map<String, String> defaultDhcpWebpaOutput = null;
	String[] arrayOfDhcpParams = { BroadBandWebPaConstants.WEB_PARAM_DELEGATED_PREFIX_IPV6,
		BroadBandWebPaConstants.WEB_PARAM_DHCPV6_BEGINNING_ADDRESS,
		BroadBandWebPaConstants.WEB_PARAM_DHCPV6_ENDING_ADDRESS };

	BroadBandResultObject broadBandResultObject = new BroadBandResultObject();
	// Variable Declaration Ends

	testCaseId = "TC-RDKB-DHCP-IPV6-103";

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-DHCP-IPV6-1003");
	LOGGER.info(
		"TEST DESCRIPTION: Verify DHCPv6 address  in Wifi connected client  on changing Min Address and Max Address ");
	LOGGER.info("PRE-CONDITION STEPS");
	LOGGER.info("PRE-CONDITION 1 : OBTAIN A 2.4GHZ WIFI CLIENT ASSOSIATED WITH THE GATEWAY");
	LOGGER.info("PRE-CONDITION 2 : RETRIEVE DHCPV6 BEGINNING ,ENDING ADDRESS USING WEBPA");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info(
		"1. Login to Lan side GUI page of device Using URL:Business Class Device:http://10.1.10.1 Residential Class Device:http://10.0.0.1");
	LOGGER.info("2. Launch Connection page and navigate to \"Connection status\"");
	LOGGER.info("3. Verify DHCPv6 Lease Time element is removed from status page");
	LOGGER.info("4. Launch Connection page and navigate to \"Local IP Network\"");
	LOGGER.info("5. Verify DHCPv6 Lease Time element is removed from status page");
	LOGGER.info("6. Change DHCPv6 Beginning Address in UI page");
	LOGGER.info("7. Change DHCPv6 Ending Address in UI page");
	LOGGER.info("8. Verfiy DHCPv6 Beginning,ending Address should be verified using webpa");
	LOGGER.info("9. Verify whether Wifi client IPv6 Address is in DHCPv6 range");
	LOGGER.info("10. Verify internet is accessibble by using intetface IPv6 on the Wifi client");

	LOGGER.info("#######################################################################################");
	try {
	    try {
		LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
		LOGGER.info("PRE-CONDITION STEPS");

		wifiConnectedClient = BroadBandPreConditionUtils.executePreConditionToVerifyWiFiClientStatus(device,
			tapEnv, BroadBandTestConstants.BAND_5GHZ, BroadBandTestConstants.CONSTANT_1);

		LOGGER.info("######################################################################################");
		LOGGER.info(
			"PRE-CONDITION 2 : DESCRIPTION : Retrieve DHCPV6 Beginning ,Ending and Lease time using Webpa");
		LOGGER.info("PRE-CONDITION 2 : ACTION : Execute webpa command to retrieve values");
		LOGGER.info("PRE-CONDITION 2 : EXPECTED : Webpa command should be executed successfully");
		LOGGER.info("######################################################################################");
		defaultDhcpWebpaOutput = tapEnv.executeMultipleWebPaGetCommands(device, arrayOfDhcpParams);
		broadBandResultObject = BroadBandWebPaUtils.verifyDhcpv6OutputValues(defaultDhcpWebpaOutput);
		status = broadBandResultObject.isStatus();
		errorMessage = broadBandResultObject.getErrorMessage();
		if (status) {
		    LOGGER.info("PRE-CONDITION 2: ACTUAL : Pre condition executed successfully");
		} else {
		    LOGGER.error("PRE-CONDITION 2: ACTUAL : Pre condition failed");
		    throw new TestException(
			    BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 2 : FAILED : " + errorMessage);
		}
		LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
		LOGGER.info("**********************************************************************************");
	    } catch (Exception e) {
		errorMessage = "Exception occured in Pre-Condition while verifying change in DHCP address in Ethernet client."
			+ e.getMessage();
		LOGGER.error(errorMessage);
	    }

	    
	    changeValuesOfDhcpv6InLanSideUI(device, wifiConnectedClient, tapEnv, testCaseId, isBrowserOpen,
		    BroadBandTestConstants.BAND_5GHZ);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    postconditionTorevertDhcpChanges(device, tapEnv, defaultDhcpWebpaOutput, BroadBandTestConstants.CONSTANT_1);
	    postConditionCloseBrowser(isBrowserOpen, BroadBandTestConstants.CONSTANT_2);
	    postConditionDisconnectSsid(device, wifiConnectedClient, tapEnv, BroadBandTestConstants.CONSTANT_3);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-DHCP-IPV6-1003");
    }
    
    /**
     * Method to change DHCPv6 Beginning & End Address and verify device Ipv6 address is within that range
     * 
     * @param device
     *            Dut instance
     * @param connectedClient
     *            Connected client instance
     * @param tapEnv
     *            AutomaticsTapApi instance
     * @param testCaseId
     *            Test Case id for which method is executing
     * @param isBrowserOpen
     *            Boolean value shows whether browser is open or not
     * @param WiFiConnectionType
     *            Shows connected client connection type(WiFi or Ethernet)
     * @refactor Said Hisham
     */
    public void changeValuesOfDhcpv6InLanSideUI(Dut device, Dut connectedClient, AutomaticsTapApi tapEnv,
	    String testCaseId, boolean isBrowserOpen, String WiFiConnectionType) {
	// Variable declaration starts
	String errorMessage = "";
	boolean status = false;
	String[] arrayOfDhcpParams = { BroadBandWebPaConstants.WEB_PARAM_DHCPV6_BEGINNING_ADDRESS,
		BroadBandWebPaConstants.WEB_PARAM_DHCPV6_ENDING_ADDRESS };
	String[] arrayofDhcpValuesToValidate = { BroadBandTestConstants.DHCPIPV6_SET_BEGINNING_ADDRESS,
		BroadBandTestConstants.DHCPIPV6_SET_ENDING_ADDRESS };
	WebDriver webDriver;
	// Variable declaration ends

	try {
	    stepNum = "S1";
	    errorMessage = "Unable to login Lan gui page ";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Login to Lan side GUI page of device in Connected client Using URL:Business Class Device:http://10.1.10.1 Residential Class Device:http://10.0.0.1");
	    LOGGER.info(
		    "STEP 1: ACTION : Login to Lan side GUI page of device  using admin/password or \"cusadmin/highspeed\" ");
	    LOGGER.info("STEP 1: EXPECTED : Login to Lan side gui page should be successful");
	    LOGGER.info("**********************************************************************************");

	    status = LanWebGuiLoginPage.logintoLanPage(tapEnv, device, connectedClient);
	    isBrowserOpen = status;
	    webDriver = LanWebGuiLoginPage.getDriver();

	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : Launch Broad band WebUI login page and verify login status is successful");

	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, false);

	    stepNum = "S2";
	    errorMessage = "Unable to navigate to Status page";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Launch Connection page and navigate to \"Connection status\"");
	    LOGGER.info(
		    "STEP 2: ACTION : Launch Connection page from GUI \"Gateway>connection \"and from Connection page navigate to Wifi page \"connection>Status\"");
	    LOGGER.info(
		    "STEP 2: EXPECTED : Connection page launch and navigate from Connection page to Status should be successful");
	    LOGGER.info("**********************************************************************************");

	    LanSidePageNavigation lanSidePageNavigation = new LanSidePageNavigation(webDriver);
	    status = lanSidePageNavigation.navigateToConnectionStatusPage(device, tapEnv, webDriver);

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Successfully navigated to Status Page ");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, false);

	    stepNum = "S3";
	    errorMessage = "DHCPv6 Lease Time element not removed from status page";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify DHCPv6 Lease Time element is removed from status page");
	    LOGGER.info("STEP 3: ACTION : verify element xpath is not present");
	    LOGGER.info("STEP 3: EXPECTED :  DHCPv6 Lease Time element should removed from status page");
	    LOGGER.info("**********************************************************************************");

	    BroadBandCommonPage commonPage = new BroadBandCommonPage(webDriver);
	    status = !commonPage.isElementByXpathAvailable(BroadBandWebGuiElements.XPATH_DHCP_LEASE_TIME);

	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : DHCPv6 Lease Time element removed from status page ");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, false);

	    stepNum = "S4";
	    errorMessage = "Unable to navigate to Local IP network";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Launch Connection page and navigate to \"Local IP Network\"");
	    LOGGER.info(
		    "STEP 4: ACTION : Launch Connection page from GUI \"Gateway>connection \"and from Connection page navigate to Wifi page \"connection>Local IP Network\"");
	    LOGGER.info(
		    "STEP 4: EXPECTED : Connection page launch and navigate from Connection page to Local IP Network should be successful");
	    LOGGER.info("**********************************************************************************");

	    status = lanSidePageNavigation.navigateToLocalIpConfigurationPage(device, tapEnv, webDriver);

	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Successfully navigated to Local IP Network Page ");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, false);

	    stepNum = "S5";
	    errorMessage = "DHCPv6 Lease Time element not removed from Local IP Network page";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify DHCPv6 Lease Time element is removed from Local IP Network page");
	    LOGGER.info("STEP 5: ACTION : verify element xpath is not present");
	    LOGGER.info("STEP 5: EXPECTED :  DHCPv6 Lease Time element should removed from Local IP Network page");
	    LOGGER.info("**********************************************************************************");

	    status = !commonPage.isElementByXpathAvailable(BroadBandWebGuiElements.XPATH_DHCP_LEASE_TIME);

	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : DHCPv6 Lease Time element removed from Local IP Network page ");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, false);

	    stepNum = "S6";
	    errorMessage = "Unable to enter values in designated text boxes";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Change DHCPv6 Ending Address in UI page");
	    LOGGER.info(
		    "STEP 6: ACTION : Enter values of DHCPv6 Ending Address to 2601:a40:300:1ee:ffff:ffff:ffff:ffffin respective text boxes");
	    LOGGER.info("STEP 6: EXPECTED : Values should be entered");
	    LOGGER.info("**********************************************************************************");
	    String[] ipv6AddressToAdd = BroadBandTestConstants.DHCPIPV6_SET_ENDING_ADDRESS
		    .split(BroadBandTestConstants.DELIMITER_COLON);
	    if (!webDriver.findElement(By.id(BroadBandWebGuiElements.ID_CHECKBOX_STATEFUL)).isSelected()) {
		webDriver.findElement(By.id(BroadBandWebGuiElements.ID_CHECKBOX_STATEFUL)).click();
	    }
	    BroadBandResultObject broadBandResultObject = BroadbandLocalIpConfigurationPage
		    .enterValuesInSeriesIntoTextBoxUsingId(webDriver, ipv6AddressToAdd,
			    BroadBandWebGuiElements.ELEMENT_ID_DEA, BroadBandTestConstants.CONSTANT_5);
	    status = broadBandResultObject.isStatus();
	    errorMessage = broadBandResultObject.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Successfully entered values in DHCPv6 Ending Address UI page ");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S7";
	    errorMessage = "Unable to enter values in designated text boxes";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Change DHCPv6 Beginning Address in UI page");
	    LOGGER.info(
		    "STEP 7: ACTION : Enter values ofDHCPv6 Beginning Address 2601:a40:300:1ee:ffff:ffff:ffff:0001 in respective text boxes");
	    LOGGER.info("STEP 7: EXPECTED : Values should be entered");
	    LOGGER.info("**********************************************************************************");
	    try {
		ipv6AddressToAdd = BroadBandTestConstants.DHCPIPV6_SET_BEGINNING_ADDRESS
			.split(BroadBandTestConstants.DELIMITER_COLON);
		broadBandResultObject = BroadbandLocalIpConfigurationPage.enterValuesInSeriesIntoTextBoxUsingId(
			webDriver, ipv6AddressToAdd, BroadBandWebGuiElements.ELEMENT_ID_DBA,
			BroadBandTestConstants.CONSTANT_5);
		status = broadBandResultObject.isStatus();
		errorMessage = broadBandResultObject.getErrorMessage();
		if (status) {
		    status = false;
		    webDriver.findElement(By.id(BroadBandWebGuiElements.ELEMENT_ID_SUBMIT_BTN_IPV6)).click();
		    status = true;
		}
	    } catch (Exception e) {
		LOGGER.error("Exception caught while saving settings in Local IP network page" + e.getMessage());
	    }
	    if (status) {
		tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);
		LOGGER.info("STEP 7: ACTUAL : Successfully entered values in DHCPv6 Beginning Address UI page ");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    BroadBandWebUiUtils.updateExecutionStatusForWebGuiStep(webDriver, tapEnv, device, testCaseId, stepNum,
		    status, errorMessage, true);

	    stepNum = "S8";
	    errorMessage = "Unable to validate values using webpa";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 8: DESCRIPTION : Verfiy DHCPv6 Beginning,ending and lease time should be verified using webpa");
	    LOGGER.info(
		    "STEP 8: ACTION : Execute command:curl -H \"Authorization: Bearer <<>>\" -k <webpa url>device/mac:<<>>/config?names=Device.IP.Interface.1.IPv6Prefix.1.Prefix,Device.DHCPv6.Server.Pool.1.PrefixRangeBegin,Device.DHCPv6.Server.Pool.1.PrefixRangeEnd,Device.DHCPv6.Server.Pool.1.LeaseTime");
	    LOGGER.info("STEP 8: EXPECTED : Values should be validated successfully");
	    LOGGER.info("**********************************************************************************");

	    Map<String, String> outputDhcpWebpaOutput = tapEnv.executeMultipleWebPaGetCommands(device,
		    arrayOfDhcpParams);
	    for (int count = 0; count < outputDhcpWebpaOutput.size(); count++) {
		status = outputDhcpWebpaOutput.get(arrayOfDhcpParams[count])
			.equalsIgnoreCase(arrayofDhcpValuesToValidate[count]);
		if (!status)
		    break;
	    }
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : Successfully validate DHCPv6 values using webpa");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S9";
	    errorMessage = "Unable to validate dhcpv6 ipaddress in connected client";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION : Verify whether Connected client IPv6 Address is in DHCPv6 range");
	    LOGGER.info(
		    "STEP 9: ACTION : Execute command:Get the device IPv6 address using below commandLinux : ifconfig wlan0 |grep -i \"inet6 addr:\"Windows:ipconfig |grep -A 10 \"Wireless LAN adapter Wi-Fi\" |grep -i \"IPv6 Address\" if IP not renewed try force renew with below commandsTo renew IP addressWindows:ipconfig /renew Linux : sudo dhclient -v -r <<Interface>>sudo dhclient -v <<Interface>>");
	    LOGGER.info("STEP 9: EXPECTED : Ipv6 address is validated successfully");
	    LOGGER.info("**********************************************************************************");

	    for (int i = BroadBandTestConstants.CONSTANT_0; i <= BroadBandTestConstants.CONSTANT_3; i++) {
		tapEnv.waitTill(BroadBandTestConstants.NINETY_SECOND_IN_MILLIS);
		if (CommonMethods.isNotNull(WiFiConnectionType)) {
		    BroadBandConnectedClientUtils.disconnectAndReconnectWiFiClient(WiFiConnectionType, tapEnv, device,
			    connectedClient);
		}
		status = BroadBandConnectedClientUtils.verifyConnectedClientIpv6AddressInDhcpAfterRenew(device, tapEnv,
			connectedClient);
		if (status) {
		    break;
		}
	    }
	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : Successfully validated Connected client IPv6 address is in DHCPv6 range");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S10";
	    errorMessage = "Unable to verify Internet connection";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 10: DESCRIPTION : Verify internet is accessibble by using interface IPv6  on the Connected client ");
	    LOGGER.info(
		    "STEP 10: ACTION : .Execute command :Linux :  curl -6 -v --interface wlan0 \"www.google.com\" | grep \"200 OK\"Windows:curl -6 -v \"www.google.com\" | grep \"200 OK\"");
	    LOGGER.info("STEP 10: EXPECTED : Connectivity check should  return the status as 200 OK.");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "NOT ABLE TO ACCESS THE SITE 'www.google.com' FROM LAN CLIENT WITH USING IPV6";
	    broadBandResultObject = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(
		    tapEnv, connectedClient,
		    BroadBandTestConstants.URL_HTTPS + BroadBandTestConstants.STRING_GOOGLE_HOST_ADDRESS,
		    BroadBandTestConstants.IP_VERSION6);
	    status = broadBandResultObject.isStatus();
	    errorMessage = broadBandResultObject.getErrorMessage();
	    if (!status) {
		errorMessage = "PING OPERATION FAILED TO ACCESS THE SITE 'www.google.com' USING IPV6 ";
		status = ConnectedNattedClientsUtils.verifyPingConnectionForIpv4AndIpv6(connectedClient, tapEnv,
			BroadBandTestConstants.PING_TO_GOOGLE, BroadBandTestConstants.IP_VERSION6);
	    }

	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : Successfully validated internet connected in connected client");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	}
    }
    
    public void postconditionTorevertDhcpChanges(Dut device, AutomaticsTapApi tapEnv,
    	    Map<String, String> defaultDhcpWebpaOutput, Integer stepNumber) {
    	BroadBandResultObject broadBandResultObject = new BroadBandResultObject();
    	boolean status = false;
    	String errorMessage;
    	if (!defaultDhcpWebpaOutput.isEmpty()) {

    	    LOGGER.info("POST-CONDITION " + stepNumber
    		    + ": DESCRIPTION : Revert back DHCP Begin,end and lease time to default values");
    	    LOGGER.info("POST-CONDITION " + stepNumber + ": ACTION : Execute webpa command with default parameters");
    	    LOGGER.info("POST-CONDITION " + stepNumber
    		    + ": EXPECTED : Webpa values should set successfully with Success 200 ok");

    	    List<WebPaParameter> webPaParameters = new ArrayList<WebPaParameter>();
    	    WebPaParameter parameterMin = BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
    		    BroadBandWebPaConstants.WEB_PARAM_DHCPV6_BEGINNING_ADDRESS,
    		    defaultDhcpWebpaOutput.get(BroadBandWebPaConstants.WEB_PARAM_DHCPV6_BEGINNING_ADDRESS),
    		    BroadBandTestConstants.CONSTANT_0);
    	    WebPaParameter parameterMax = BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
    		    BroadBandWebPaConstants.WEB_PARAM_DHCPV6_ENDING_ADDRESS,
    		    defaultDhcpWebpaOutput.get(BroadBandWebPaConstants.WEB_PARAM_DHCPV6_ENDING_ADDRESS),
    		    BroadBandTestConstants.CONSTANT_0);

    	    webPaParameters.add(parameterMin);
    	    webPaParameters.add(parameterMax);
    	    broadBandResultObject = BroadBandWebPaUtils.executeSetAndGetOnMultipleWebPaGetParams(device, tapEnv,
    		    webPaParameters);
    	    status = broadBandResultObject.isStatus();
    	    errorMessage = broadBandResultObject.getErrorMessage();

    	    if (status) {
    		LOGGER.info("POST-CONDITION " + stepNumber + ": ACTUAL : Post condition executed successfully");
    	    } else {
    		LOGGER.error("POST-CONDITION " + stepNumber + ": ACTUAL : Post condition failed :" + errorMessage);
    	    }
    	}
        }

        /**
         * Method postcondition to close browser
         * 
         * @param isBrowserOpen
         * @param stepNumber
         * @refactor Said Hisham
         */
        public void postConditionCloseBrowser(boolean isBrowserOpen, Integer stepNumber) {

    	/**
    	 * Post-condition 1 : Close the LAN Side Browser
    	 */
    	LOGGER.info("POST-CONDITION " + stepNumber + " : DESCRIPTION : VERIFY LAN SIDE BROWSER CLOSED");
    	LOGGER.info("POST-CONDITION " + stepNumber + " : ACTION : CLOSE THE LAN SIDE BROWSER");
    	LOGGER.info("POST-CONDITION " + stepNumber + " : EXPECTED : BROWSER CLOSED SUCCESSFULLY IN LAN SIDE");
    	if (isBrowserOpen) {
    	    try {
    		LanSideBasePage.closeBrowser();
    		LOGGER.info("POST-CONDITION " + stepNumber + " : ACTUAL : BROWSER CLOSED SUCCESSFULLY.");
    	    } catch (Exception exception) {
    		LOGGER.info("POST-CONDITION " + stepNumber
    			+ " : ACTUAL : EXCEPTION OCCURRED WHILE CLOSING THE BROWSER, UNABLE TO CLOSE BROWSER.");
    	    }
    	} else {
    	    LOGGER.info("POST-CONDITION " + stepNumber + " : ACTUAL : BROWSER NOT OPENED IN LAN SIDE.");
    	}
        }


    
    
	
	


}
